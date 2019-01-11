(require 'cl-lib)
(require 'cl-macs)
;; (require 'eieio)

(cl-defstruct autobuild-rule
  ;; name
  major-modes
  nice
  genaction
  action-async-p)

(defvar autobuild-rules-alist nil)
;; (setq autobuild-rules-alist nil)

(defun autobuild-add-rule (name rule)
  ;; (assq-delete-all name autobuild-rules)
  (setf (alist-get name autobuild-rules-alist) rule))

(defvar autobuild-directives '(autobuild-nice autobuild-async))

(defalias 'autobuild-nice #'ignore)
(defalias 'autobuild-async #'ignore)

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
        (nice (or (alist-get 'autobuild-nice directives) 10))
        (async (or (alist-get 'autobuild-async directives) t)))
    `(autobuild-add-rule
      ',name
      (make-autobuild-rule
       ;; :name ',name
       :major-modes ',major-modes
       :nice ,nice
       :genaction (defun ,name () ,@body)
       :action-async-p ,async))))


(defvar autobuild-rules-remaining nil)
(make-variable-buffer-local 'autobuild-rules-remaining)
(defvar autobuild-rules-remaining-global nil)

(defun autobuild-pipeline (rule-names)
  (cl-loop for rules-remaining on rule-names
           do
           (destructuring-bind (buffer name) (car rules-remaining)
             (with-current-buffer buffer
               (let* ((rule (alist-get name autobuild-rules-alist))
                      (action (funcall (autobuild-rule-genaction rule))))
                 (assert action)
                 ;; TODO fail early on non-zero exit, error
                 ;; or ensure each action errs
                 (if (and (cdr rules-remaining)
                          (or (stringp action)
                              (autobuild-rule-action-async-p rule)))
                     (progn
                       (setq autobuild-rules-remaining-global (cdr rules-remaining))
                       (autobuild-run-action action)
                       (return))
                   (autobuild-run-action action)))))))

(defun autobuild-pipeline-continue-schedule (proc)
  ;; TODO use proc vars instead of buffer-local var
  (when autobuild-rules-remaining-global
    (message "scheduling remaining rules: %s" autobuild-rules-remaining-global))
  (setq autobuild-rules-remaining autobuild-rules-remaining-global
        autobuild-rules-remaining-global nil))

(add-hook 'compilation-start-hook #'autobuild-pipeline-continue-schedule)

(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defun autobuild-pipeline-continue (compilation-buffer finish-state)
  ;; (edebug)
  (with-current-buffer compilation-buffer
    (when autobuild-rules-remaining
      (if (compilation-exited-abnormally-p finish-state)
          (progn
            (message "aborting pipeline: %s" autobuild-rules-remaining)
            (setq autobuild-rules-remaining nil))
        (progn
          (message "continuing with pipeline: %s" autobuild-rules-remaining)
          (autobuild-pipeline autobuild-rules-remaining))))))

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

(defcustom selcand-default-hints
  "qwertasdfzxcv1234"
  "Default hint chars."
  :type 'string
  :group 'selcand)

(defun selcand-hints (cands &optional chars)
  "Return an alist (HINT . CAND) for each candidate in CANDS.

  each hint consists of characters in the string CHARS."
  (setf chars (or chars selcand-default-hints))
  (assert cands)
  (let* ((w (ceiling (log (length cands) (length chars))))
         (hints (cl-loop with curr = '("")
                         for wi below w do
                         (setf curr
                               (cl-loop for c across chars
                                        append (mapcar (apply-partially
                                                        'concat (char-to-string c))
                                                       curr)))
                         finally (return curr))))
    (cl-loop for hint in hints
             for cand in cands
             collect (cons hint cand))))

(defun selcand-select (cands &optional prompt stringify)
  "Use PROMPT to prompt for a selection from CANDS candidates."
  (let* ((hints-cands (selcand-hints cands))
         (sep ") ")
         (stringify (or stringify #'prin1-to-string))
         (choices (cl-loop for (hint . cand) in hints-cands
                           collect (concat hint sep (funcall stringify cand))))
         (prompt (or prompt "select candidate: "))
         (choice (completing-read prompt choices
                                  nil
                                  t))
         (cand (let* ((hint (car (s-split sep choice))))
                 (cdr (assoc hint hints-cands #'equal)))))
    cand))

(global-set-key (kbd "M-c") #'autobuild-build)

(defvar autobuild-last-rule nil)
(make-variable-buffer-local 'autobuild-last-rule)
;; (setq-default )

(defun sort-by (key list)
  (sort list (lambda (a b) (< (funcall key a) (funcall key b)))))

(defun autobuild-build (&optional prompt)
  (interactive "P")
  (let* ((cands (autobuild-current-build-actions))
         (choice (if (and prompt)
                     (selcand-select cands "select build rule: "
                                            (lambda (name-rule-action)
                                              (format "%s"
                                                      (car name-rule-action))))
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

(defun autobuild-run-string-command (cmd)
  (let ((compile-command cmd)
        (emacs-filename-env-directive
         ;; allow compile commands to use rename-proof filename
         (concat "AUTOBUILD_FILENAME=" (buffer-file-name (current-buffer)))))
    (push emacs-filename-env-directive process-environment)
    ;; TODO decouple this from autobuild
    (let ((ansi-color-for-comint-mode t))
      (compile cmd))))

(defun file-local-compile-command ()
  ;;(read-file-local-variable-value 'compile-command)
  ;;TODO read file local compile-command
  (cdr (assoc 'compile-command file-local-variables-alist)))

(autobuild-define-rule
 autobuild-file-local-compile-command
 t
 ;; compile-command ;; this includes the default "make -k"
 "A rule that matches any buffer whose compile-command is set"
 ;; make sure there is a custom compile command
 (file-local-compile-command))

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
 (when (buffer-file-name)
   (let ((fn (f-filename (buffer-file-name))))
     (if (and fn (file-executable-p fn))
         (format "./%s" fn)))))

(autobuild-define-rule autobuild-dired-build-file-at-point
                       (dired-mode)
                       (with-temporary-current-file
                        (dired-file-name-at-point)
                        (call-interactively 'erjoalgo-compile-compile)
                        '(abort)))

(autobuild-define-rule autobuild-shell-script
                       (sh-mode)
                       (let ((fn (f-filename (buffer-file-name))))
                         (format "bash %s" fn)))

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
                       (when (equal "pkgdef" (f-ext (buffer-file-name (current-buffer))))
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
                         (browser-new-tab url)))

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
