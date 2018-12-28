(require 'cl-lib)
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

(cl-defmacro autobuild-define-rule (major-modes
                                    body
                                    &key
                                    (name (gensym "autobuild-anon-rule-"))
                                    (nice 10)
                                    doc)
  (cl-assert major-modes)
  `(autobuild-add-rule
    ',name
    (make-autobuild-rule
     ;; :name ',name
     :major-modes ',major-modes
     :nice ,nice
     :genaction (lambda () ,body))))

(defun autobuild-pipeline (&rest rule-names)
  (cl-loop for name in rule-names
           as rule = (alist-get autobuild-rules-alist name)
           as action = (autobuild-rule-action rule)
           ;; TODO fail early on non-zero exit, error
           ;; or ensure each action errs
           as ret = (autobuild-run-action action)))

;; (defmacro with-slots (slots obj &rest body)
;;   `(let (loop for slot in slots
;;               collect `(slot (slot-value ,obj ',slot))))


(defun autobuild-current-build-actions ()
  (cl-loop for (name . rule) in autobuild-rules-alist
           as nice = (autobuild-rule-nice rule)
           as action =
           ;; (with-slots (major-modes genaction) rule
           (let ((major-modes (autobuild-rule-major-modes rule))
                 (genaction (autobuild-rule-genaction rule)))
             (and
              (or
               (eq t major-modes)
               (if (atom major-modes)
                   (eq major-mode major-modes)
                 (find major-mode major-modes)))
              ;; (funcall genaction filename)
              (funcall genaction)))
           when action
           collect (cons nice action) into cands
           ;; finally (return (sort-by #'autobuild-rule-nice rules)))
           finally (return (mapcar #'cdr (sort-by #'car cands)))))

(defcustom selcand-default-hints
  "qwertasdfzxcv1234"
  "Default hint chars."
  :type 'string
  :group 'selcand)

(defun selcand-hints (cands &optional chars)
  "Return an alist (HINT . CAND) for each candidate in CANDS.

  each hint consists of characters in the string CHARS."
  (setf chars (or chars selcand-default-hints))
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

(defun selcand-select (cands &optional prompt)
  "Use PROMPT to prompt for a selection from CANDS candidates."
  (let* ((hints-cands (selcand-hints cands))
         (sep ") ")
         (choices (cl-loop for (hint . cand) in hints-cands
                           collect (concat hint sep (prin1-to-string cand))))
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
         (choice (if (and prompt (cdr cands))
                     (selcand-select cands "select build rule: ")
                   (or autobuild-last-rule (car cands)))))
    (if (null choice)
        (error "No build rules matched")
      (setq autobuild-last-rule choice)
      (autobuild-run-action choice))))

(defun autobuild-run-action (action)
  (assert action)
  (cond
   ((stringp action) (autobuild-run-string-command action))
   ((commandp action) (call-interactively action))
   ((functionp action) (funcall action))
   (t (error "Action must be string or function"))))

(defun autobuild-run-string-command (cmd)
  (let ((compile-command cmd)
        (emacs-filename-env-directive
         ;; allow compile commands to use rename-proof filename
         (concat "AUTOBUILD_FILENAME=" (buffer-file-name (current-buffer)))))
    (push emacs-filename-env-directive process-environment)
    ;; TODO decouple this from autobuild
    (let ((ansi-color-for-comint-mode t))
      (compile cmd))))

(defun autobuild-file-local-compile-command ()
  ;;(read-file-local-variable-value 'compile-command)
  ;;TODO read file local compile-command
  (cdr (assoc 'compile-command file-local-variables-alist)))

(autobuild-define-rule
 t
 ;; compile-command ;; this includes the default "make -k"
 (autobuild-file-local-compile-command) ;; make sure there is a custom compile command
 :doc "A rule that matches any buffer whose compile-command is set")

(autobuild-define-rule
 t
 (when (s-starts-with?
        (expand-file-name "~/git/translations")
        (buffer-file-name))
   (lambda ()
     (when (translation-prepare)
       (call-interactively #'translation-publish-commit)))))

(autobuild-define-rule
 fundamental-mode
 ;;git commit
 (progn
   (save-buffer)
   (server-edit)))

(autobuild-define-rule t
                       (when (buffer-file-name)
                         (let ((fn (f-filename (buffer-file-name))))
                           (if (and fn (file-executable-p fn))
                               (format "./%s" fn)))))

(autobuild-define-rule dired-mode
                       (with-temporary-current-file
                        (dired-file-name-at-point)
                        (call-interactively 'erjoalgo-compile-compile)
                        '(abort)))

(autobuild-define-rule
 sh-mode
 (let ((fn (f-filename (buffer-file-name))))
   (format "bash %s" fn)))

(autobuild-define-rule (java-mode nxml-mode)
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

(autobuild-define-rule lisp-mode 'slime-compile-and-load-file)

(autobuild-define-rule
 emacs-lisp-mode
 (if (and (buffer-file-name)
          (s-ends-with-p "-tests.el" (buffer-file-name)))
     (lambda () (eval-buffer) (ert t))
   'eval-buffer))

(autobuild-define-rule t
                       (when (file-exists-p "Makefile") "make"))

(autobuild-define-rule t
                       (when (equal "pkgdef" (f-ext (buffer-file-name (current-buffer))))
                         (format "mpm build --pkgdef_file=%s --alsologtostderr"
                                 (buffer-file-name (current-buffer)))))

(autobuild-define-rule
 c-mode
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" ""))
       (speed (if (and (boundp 'c-ofast-compilation) c-ofast-compilation)
                  "-Ofast" "-g")))
   (format "gcc %s -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s"
           speed fn pipe-in)))

(autobuild-define-rule
 c++-mode
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
   (format "g++ %s -std=c++11 && ./a.out %s"
           fn pipe-in)))

(autobuild-define-rule
 go-mode
 "go test")

(autobuild-define-rule (tex-mode latex-mode) 'latex-compile)

(autobuild-define-rule
 python-mode
 (format "python %s" (f-filename (buffer-file-name))))

(autobuild-define-rule
 (git-rebase-mode text-mode)
 (progn
   (save-buffer)
   (with-editor-finish nil)))

(autobuild-define-rule
 diff-mode
 (progn (save-buffer)
        (server-edit)))

(autobuild-define-rule (clojure-mode) 'cider-load-buffer)

(autobuild-define-rule (message-mode) 'message-send-and-exit)

(autobuild-define-rule (org-mode) 'org-export-mine)

(autobuild-define-rule (octave-mode)
                       (call-interactively
                        (if (region-active-p)
                            'octave-send-region
                          'octave-send-buffer)))

(autobuild-define-rule (html-mode mhtml-mode)
                       (let ((url
                              (->> (buffer-file-name)
                                   (sanitize-filename)
                                   (format "file://%s"))))
                         (browser-new-tab url)))

(autobuild-define-rule
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (format "node %s" filename)))

(autobuild-define-rule
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (when (s-ends-with-p "-boot.json" filename)
     (format "cf-boot %s -i free-vars.json" filename))))

(autobuild-define-rule
 (texinfo-mode)
 (concat "texi2any ${EMACS_COMPILATION_FILENAME}"
         " --html"
         " --no-number-sections"))

(autobuild-define-rule
 (nginx-mode)
 (concat "sudo service nginx restart"))
