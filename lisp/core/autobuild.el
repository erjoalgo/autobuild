(require 'cl-lib)
(require 'selcand)
(require 'cl-macs)
;; (require 'eieio)

(cl-defstruct autobuild-rule
  ;; name
  major-modes
  nice
  genaction)

(defvar autobuild-rules-alist nil)
;; (setq autobuild-rules-alist nil)

(defun autobuild-add-rule (name rule)
  ;; (assq-delete-all name autobuild-rules)
  (setf (alist-get name autobuild-rules-alist) rule))

(defvar autobuild-directives '(autobuild-nice))
(defalias 'autobuild-nice #'ignore)

(cl-defmacro autobuild-define-rule (name
                                    major-modes
                                    &rest body)
  (cl-assert major-modes)
  (when (and (not (eq t major-modes))
             (member t major-modes))
    (error "invalid major mode specification"))
  (let* ((directives
          (loop for top-level-form in body
                ;; TODO remove directives from body
               when (and (listp top-level-form)
                         (member (car top-level-form) autobuild-directives))
               collect (cons (car top-level-form)
                             (cadr top-level-form))))
        (nice (or (alist-get 'autobuild-nice directives) 10)))
    `(autobuild-add-rule
      ',name
      (make-autobuild-rule
       ;; :name ',name
       :major-modes ',major-modes
       :nice ,nice
       :genaction (defun ,name () ,@body)))))


(defvar-local autobuild-rules-remaining nil)

(defmacro autobuild-pipeline (&rest buffer-rule-list)
  `(lambda ()
     (autobuild-pipeline-run
      (list ,@(cl-loop for (buffer rule-name) in buffer-rule-list
                       collect `(list ,buffer ,rule-name))))))

(defun autobuild-pipeline-run (rules-remaining)
  (when rules-remaining
    (destructuring-bind (buffer name) (car rules-remaining)
      (with-current-buffer buffer
        (let* ((rule (alist-get name autobuild-rules-alist))
               (action (funcall (autobuild-rule-genaction rule))))
          (assert action)
          (let ((result (autobuild-run-action action)))
            (if (and (bufferp result)
                     (eq 'compilation-mode (buffer-local-value 'major-mode result)))
                (progn
                  (autobuild-compilation-buffer-setup result (current-buffer))
                  (with-current-buffer result
                    (setq autobuild-rules-remaining (cdr rules-remaining))
                    (message "scheduling remaining rules: %s" autobuild-rules-remaining)))
              (progn
                ;; TODO fail early on non-zero exit, error
                ;; or ensure each action errs
                (setq autobuild-rules-remaining (cdr rules-remaining))
                (autobuild-pipeline-run (cdr rules-remaining))))))))))


(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defun autobuild-pipeline-continue (compilation-buffer finish-state)
  ;; (edebug)
  (with-current-buffer compilation-buffer
    (when (bound-and-true-p autobuild-rules-remaining)
      (if (compilation-exited-abnormally-p finish-state)
          (progn
            (message "aborting pipeline: %s" autobuild-rules-remaining)
            (setq autobuild-rules-remaining nil))
        (progn
          (message "continuing with pipeline: %s" autobuild-rules-remaining)
          (autobuild-pipeline-run autobuild-rules-remaining))))))

(add-hook 'compilation-finish-functions #'autobuild-pipeline-continue)

(defun autobuild-current-build-actions ()
  (cl-loop for (name . rule) in autobuild-rules-alist
           as action =
           (let ((major-modes (autobuild-rule-major-modes rule))
                 (genaction (autobuild-rule-genaction rule)))
             (and
              (or
               (eq t major-modes)
               (if (atom major-modes)
                   (eq major-mode major-modes)
                 (find major-mode major-modes)))
              (funcall genaction)))
           when action
           collect (list name rule action) into cands
           finally (return (sort-by (lambda (rule-action)
                                      (autobuild-rule-nice (cadr rule-action)))
                                    cands))))

(global-set-key (kbd "M-c") #'autobuild-build)

(defvar-local autobuild-last-rule nil)

(defun sort-by (key list)
  (sort list (lambda (a b) (< (funcall key a) (funcall key b)))))

(defun autobuild-build (&optional prompt)
  (interactive "P")
  (let* ((cands (autobuild-current-build-actions))
         (choice (if (and prompt)
                     (selcand-select cands "select build rule: "
                                            (lambda (name-rule-action)
                                              (format "%s (%s)"
                                                      (car name-rule-action)
                                                      (autobuild-rule-nice
                                                       (cadr name-rule-action)))))
                   (or autobuild-last-rule (car cands)))))
    (if (null choice)
        (error "No build rules matched")
      (setq autobuild-last-rule choice)
      (autobuild-run-action (caddr choice)))))

(defun autobuild-run-action (action)
  (assert action)
  (cond
   ((stringp action) (autobuild-run-string-command action))
   ((commandp action) (call-interactively action))
   ((functionp action) (funcall action))
   (t (error "Action must be string or function, not %s" action))))

(defvar-local autobuild-compilation-start-time nil)

(defvar-local autobuild-last-compilation-buffer nil)

(defun autobuild-run-string-command (cmd)
  (let ((emacs-filename-env-directive
         ;; allow compile commands to use rename-proof filename
         (concat "AUTOBUILD_FILENAME=" (buffer-file-name (current-buffer)))))
    (push emacs-filename-env-directive process-environment)
    ;; TODO decouple this from autobuild
    (let* ((ansi-color-for-comint-mode t)
           (compilation-buffer (compile cmd)))
      (autobuild-compilation-buffer-setup
       compilation-buffer (current-buffer)))))

(defun autobuild-compilation-buffer-setup (compilation-buffer &optional
                                                              original-buffer cmd)
  (when original-buffer
    (with-current-buffer original-buffer
      (setq autobuild-last-compilation-buffer compilation-buffer)))
  (with-current-buffer compilation-buffer
    (setq autobuild-compilation-start-time (time-to-seconds)
          compile-command (or compile-command cmd))))

(provide 'autobuild)

;; default rules and examples
(autobuild-define-rule
 autobuild-file-local-compile-command
 t
 ;; compile-command ;; this includes the default "make -k"
 "A rule that matches any buffer whose compile-command is set"
 ;; make sure there is a custom compile command
 (autobuild-nice 9)
 (cdr (assoc 'compile-command file-local-variables-alist)))

(autobuild-define-rule
 autobuild-translations
 t
 (when (s-starts-with?
        (expand-file-name "~/git/translations")
        (buffer-file-name))
   (lambda ()
     (when (translation-prepare)
       (call-interactively #'translation-publish-commit)))))

(autobuild-define-rule
 autobuild-git-commit
 (fundamental-mode)
 (save-buffer)
 (server-edit))

(autobuild-define-rule
 autobuild-run-executable
 t
 (autobuild-nice 9)
 (when (buffer-file-name)
   (let ((fn (f-filename (buffer-file-name))))
     (if (and fn (file-executable-p fn))
         (format "./%s" fn)))))

(autobuild-define-rule autobuild-dired-build-file-at-point
                       (dired-mode)
                       (when (dired-file-name-at-point)
                         (lambda ()
                           (with-temporary-current-file
                            (dired-file-name-at-point)
                            (call-interactively #'autobuild-build)))))

(autobuild-define-rule autobuild-shell-script
                       (sh-mode)
                       (let ((fn (f-filename (buffer-file-name))))
                         (format "bash %s" fn)))

(autobuild-define-rule autobuild-shell-script-syntax-check
                       (sh-mode)
                       (autobuild-nice 15)
                       (let ((fn (f-filename (buffer-file-name))))
                         (format "bash -n %s" fn)))

(autobuild-define-rule autobuild-java-mode
                       (java-mode nxml-mode)
                       (when (or (eq 'java-mode major-mode)
                                 (and (buffer-file-name)
                                      (equal (f-filename (buffer-file-name)) "pom.xml")))

                         (let ((f-no-ext
                                (-> (buffer-file-name) (f-filename) (f-no-ext)))
                               (pom-directory (cl-loop for dir in (walk-up-directories
                                                                   default-directory)
                                                       thereis (and
                                                                (member "pom.xml"
                                                                        (directory-files dir))
                                                                dir))))
                           (if (not pom-directory)
                               (format "javac %s.java && java %s" f-no-ext f-no-ext)
                             (concat "cd " pom-directory " && mvn "
                                     ;;maybe add offline flag
                                     (when (and (boundp 'mvn-offline-p) mvn-offline-p) "-o ")
                                     ;;always clean
                                     "clean "
                                     ;; verify or install
                                     (cond
                                      ((s-ends-with-p "IT" f-no-ext) "verify ")
                                      (t "install "))
                                     ;;maybe add -s *_settings.xml
                                     (let* ((mvn-settings (remove-if-not
                                                           (lambda (filename)
                                                             (s-ends-with-p "settings.xml" filename))
                                                           (directory-files pom-directory)))
                                            (mvn-settings (car mvn-settings)))
                                       (when mvn-settings (concat "-s " mvn-settings " ")))
                                     ;;maybe add proxy opts
                                     (let ((jvm-proxy (let ((https (cdr (assoc "https" url-proxy-services))))
                                                        (if (and https (s-contains? ":" https))
                                                            (apply 'format "-Dhttps.proxyHost=%s -Dhttps.proxyPort=%s"
                                                                   (split-string https ":" t))
                                                          ""))))
                                       (when jvm-proxy (concat jvm-proxy " ")))

                                     (when (and (boundp 'mvn-extra-args)
                                                mvn-extra-args) (concat mvn-extra-args " ")))))))

(autobuild-define-rule autobuild-cl-slime-eval (lisp-mode)
                       #'slime-compile-and-load-file)

(autobuild-define-rule autobuild-cl-asdf (lisp-mode)
                       (let ((filename (f-filename (buffer-file-name))))
                         (when (member (f-ext filename) '("asd" "asdf"))
                           (format "sbcl --load %s --eval \"(ql:quickload '%s)\""
                                   filename
                                   (f-base filename)))))


(autobuild-define-rule autobuild-cl (lisp-mode) 'slime-compile-and-load-file)

(autobuild-define-rule autobuild-el-eval-buffer
 (emacs-lisp-mode)
 (if (and (buffer-file-name)
          (s-ends-with-p "-tests.el" (buffer-file-name)))
     (lambda () (eval-buffer) (ert t))
   #'eval-buffer))

(autobuild-define-rule autobuild-makefile
                       t
                       (when (file-exists-p "Makefile") "make"))

(autobuild-define-rule autobuild-mpm t
                       (when (and (buffer-file-name)
                                  (equal "pkgdef" (f-ext (buffer-file-name))))
                         (format "mpm build --pkgdef_file=%s --alsologtostderr"
                                 (buffer-file-name (current-buffer)))))

(autobuild-define-rule
 autobuild-c
 (c-mode)
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" ""))
       (speed (if (and (boundp 'c-ofast-compilation) c-ofast-compilation)
                  "-Ofast" "-g")))
   (format "gcc %s -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s"
           speed fn pipe-in)))

(autobuild-define-rule autobuild-c++
 (c++-mode)
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
   (format "g++ %s -std=c++11 && ./a.out %s"
           fn pipe-in)))

(autobuild-define-rule
 autobuild-go
 (go-mode)
 "go test")

(autobuild-define-rule autobuild-latex
                       (tex-mode latex-mode)
                       'latex-compile)

(autobuild-define-rule
 autobuild-python-run
 (python-mode)
 (format "python %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-git-finish
 (git-rebase-mode text-mode)
 (progn
   (save-buffer)
   (with-editor-finish nil)))

(autobuild-define-rule autobuild-diff
 (diff-mode)
 (progn (save-buffer)
        (server-edit)))

(autobuild-define-rule autobuild-clojure (clojure-mode) 'cider-load-buffer)

(autobuild-define-rule autobuild-send-message
                       (message-mode)
                       'message-send-and-exit)

(autobuild-define-rule autobuild-org-export
                       (org-mode) 'org-export-mine)

(autobuild-define-rule autobuild-octave-eval
                       (octave-mode)
                       (call-interactively
                        (if (region-active-p)
                            'octave-send-region
                          'octave-send-buffer)))

(autobuild-define-rule autobuild-html-browse
                       (html-mode mhtml-mode)
                       (let ((url
                              (->> (buffer-file-name)
                                   (sanitize-filename)
                                   (format "file://%s"))))
                         (apply-partially #'browser-new-tab url)))

(autobuild-define-rule autobuild-node-run
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (format "node %s" filename)))

(autobuild-define-rule autobuild-cfboot
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (when (s-ends-with-p "-boot.json" filename)
     (format "cf-boot %s -i free-vars.json" filename))))

(autobuild-define-rule autobuild-texinfo-build
 (texinfo-mode)
 (concat "texi2any ${EMACS_COMPILATION_FILENAME}"
         " --html"
         " --no-number-sections"))

(autobuild-define-rule autobuild-nginx-restart
 (nginx-mode)
 (concat "sudo service nginx restart"))
