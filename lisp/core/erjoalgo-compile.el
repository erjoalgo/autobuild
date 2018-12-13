(defvar erjoalgo-compile-last-compilation-start-time nil
  "The start time in seconds since epoch of the last compilation")

(defvar erjoalgo-compile-command-queue nil
  "Internal. Keep track of the next command in the global compilation pipeline")

(defvar erjoalgo-compile-original-compile-buffer nil
  "Internal. Keep track of the original buffer
where the global compilation pipeline was invoked")

(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defun erjoalgo-compile-next-cmd (compilation-buffer compilation-state)
  "‘compilation-finish-functions' hook function
 invoked after compilation to resume pipeline.
If COMPILATION-STATE indicates compilation exited abnormally,
the pipeline is aborted."
  (cond
   ((compilation-exited-abnormally-p compilation-state)
    (message "aborting the rest of the compilation pipeline...")
    (setf erjoalgo-compile-command-queue '(abort)))
   ((null erjoalgo-compile-command-queue)
    ;; call with a dummy synchronous function to possibly trigger pipeline finished hooks
    ;; like notifications
    (setf erjoalgo-compile-command-queue '(ignore))))
  (erjoalgo-compile-compile nil erjoalgo-compile-command-queue compilation-buffer compilation-state))

(add-hook 'compilation-finish-functions 'erjoalgo-compile-next-cmd)

(defvar erjoalgo-compile-pipeline-finished-hook nil
  "Hook called when the entire compilation pipeline has completed")

(defun erjoalgo-compile-compile (arg &optional cmd-list
                                     compilation-finish-function-buffer
                                     compilation-finish-function-message)
  (interactive "P")
  (if (and arg compile-command)
      (recompile)

    (unless cmd-list
      ;; just called interactively, not recursively from list

      ;; determine compile command
      (setf cmd-list
	    (or
	     (and compile-command-set-interactively compile-command)
             ;;file-local
	     (erjoalgo-compile-read-file-local-cmd-list)
             ;;matcher
	     (erjoalgo-compile-cmd-for-current-buffer)
             ;;ask user and save
	     (call-interactively 'erjoalgo-compile-set-cmd '(4))))

      ;; maybe coerce to list
      (when (or (atom cmd-list)
		(functionp cmd-list)
                (functionp (car cmd-list))
                (eq (car cmd-list) 'async))
        (setf cmd-list (list cmd-list)))

      ;; set start time
      (setf erjoalgo-compile-last-compilation-start-time (time-to-seconds))
      (setf erjoalgo-compile-original-compile-buffer (current-buffer)))

    (let ((default-directory
            (buffer-local-value 'default-directory
                                erjoalgo-compile-original-compile-buffer))
          asyncp
          abort)
      (loop while cmd-list
            as cmd = (pop cmd-list)
            for i from 1
            ;; thereis to allow short-circuiting
            thereis
            (progn
              (when (and (consp cmd) (eq 'async (car cmd)))
                (setf asyncp t
                      cmd (cdr cmd)))
	      (cond
	       ((stringp cmd)
                ;; will run a subshell. add EMACS_COMPILATION_FILENAME env var
                (let ((compile-command cmd)
	              (emacs-filename-env-directive
	               (concat "EMACS_COMPILATION_FILENAME=" (buffer-file-name (current-buffer)))))

                  (push emacs-filename-env-directive process-environment)
                  (let ((ansi-color-for-comint-mode t))
                    (with-split-preference t
	                                   (compile cmd)))
                  ;; break, since (compile cmd) is async...
                  ;; update 'erjoalgo-compile-command-queue and continue remaining commands
                  ;; via 'erjoalgo-compile-next-cmd hook
                  (setf asyncp t)))
	       ((commandp cmd) (call-interactively cmd))
	       ((functionp cmd) (funcall cmd))
	       ((functionp (car-safe cmd)) (eval cmd) (message "completed %s" cmd))
               ((eq 'abort cmd) (setf abort t))
	       ((null cmd) (error "no compile command found for this buffer"))
	       (t (error (typecase cmd
                           (symbol (error "the function %s is undefined" cmd))
                           (t "cmd must be a function or string, not %s" cmd)))))
              asyncp))

      (setf erjoalgo-compile-command-queue cmd-list)

      (when (and (not asyncp)
                 (null erjoalgo-compile-command-queue))
        ;; done with pipeline
        (run-hook-with-args 'erjoalgo-compile-pipeline-finished-hook
                            compilation-finish-function-buffer
                            compilation-finish-function-message)
        (unless (or abort
                    (not (buffer-live-p erjoalgo-compile-original-compile-buffer)))
          ;; allow chaining by possibly starting compilation on another buffer
          (with-current-buffer erjoalgo-compile-original-compile-buffer
            (when (and (boundp 'erjoalgo-compilation-next-buffer)
	               erjoalgo-compilation-next-buffer)
              (when
                  (or
                   (get-buffer erjoalgo-compilation-next-buffer)
                   (find-file erjoalgo-compilation-next-buffer))
                (when (eq (current-buffer)
                          (get-buffer erjoalgo-compilation-next-buffer))
                  (error "compilation cycle"))
                (progn
                  (with-current-buffer erjoalgo-compilation-next-buffer
	            (erjoalgo-compile-compile arg)))))))))))

(make-variable-buffer-local 'erjoalgo-compilation-next-buffer)
(setf compilation-save-buffers-predicate (lambda () nil))

(defvar-local compile-command-set-interactively nil)
(defvar-local erjoalgo-compilation-next-buffer nil)

(put 'compile-command 'safe-local-variable 'stringp)
(defun erjoalgo-compile-read-file-local-cmd-list ()
  ;;(read-file-local-variable-value 'compile-command)
  ;;TODO read file local compile-command
  (cdr (assoc 'compile-command file-local-variables-alist)))

(defun erjoalgo-compile-cmd-for-current-buffer ()
  (loop for matcher in erjoalgo-compile-cmd-for-buffer
	thereis (funcall matcher)))

(defvar compile-command-matchers-list ()
  "Each entry in this list is a matcher
whose value should be the compile-command to use
for the current buffer, or nil if matcher does not
know how to compile the current buffer.
It is called with no arguments and with the
buffer where compilation has been requested as current.")

(defun erjoalgo-compile-set-cmd (cmd)
  (interactive
   (list
    (if current-prefix-arg;; read-lisp-object
	(read--expression "enter compile command sexp: ")
      (read-shell-command "enter compile command: "
			  (if (and (boundp 'compile-command-set)
                                   (or compile-command-set
                                       (assoc 'compile-command file-local-variables-alist)))
                              compile-command
                            (let ((match (erjoalgo-compile-cmd-for-current-buffer)))
                              (if (and match (stringp match))
                                  match "")))))))

  (add-file-local-variable 'compile-command cmd)
  (setf compile-command cmd)
  (setf compile-command-set-interactively t))

(defun erjoalgo-compile-set-next-buffer (next-buffer)
  (interactive "benter next buffer to compile: ")
  ;; (add-file-local-variable 'erjoalgo-compile-next-buffer next-buffer)
  (add-file-local-variable 'erjoalgo-compilation-next-buffer next-buffer)
  (setf erjoalgo-compilation-next-buffer next-buffer))

(defmacro buffer-major-mode-matcher (modes &rest forms)
  `(lambda ()
     (when ,(if (atom modes) `(eq major-mode ',modes)
              `(member major-mode ',modes))
       ,@forms)))

(defun walk-up-directories (dir)
  (loop with dir = default-directory
	while dir
	collect dir
	do (setf dir (f-dirname dir))))

(setf
 erjoalgo-compile-cmd-for-buffer
 (list

  (lambda ()
    (when (s-starts-with?
           (expand-file-name "~/git/translations")
           (buffer-file-name))
      (lambda ()
        (when (translation-prepare)
          (call-interactively #'translation-publish-commit)))))

  (buffer-major-mode-matcher
   fundamental-mode
   ;;git commit
   (lambda ()
     (save-buffer)
     (server-edit)))

  (lambda ()
    (when (buffer-file-name)
      (let ((fn (f-filename (buffer-file-name))))
        (if (and fn (file-executable-p fn))
            (format "./%s" fn)))))

  (buffer-major-mode-matcher dired-mode
                             (with-temporary-current-file
                              (dired-file-name-at-point)
                              (call-interactively 'erjoalgo-compile-compile)
                              '(abort)))

  (buffer-major-mode-matcher
   sh-mode
   (let ((fn (f-filename (buffer-file-name))))
     (format "bash %s" fn)))

  (lambda ()
    (when (or (eq 'java-mode major-mode)
	      (and (buffer-file-name)
                   (equal (f-filename (buffer-file-name)) "pom.xml")))

      (let ((f-no-ext
	     (-> (buffer-file-name) (f-filename) (f-no-ext)))
	    (pom-directory (loop for dir in (walk-up-directories
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

  (buffer-major-mode-matcher 'lisp-mode 'slime-compile-and-load-file)

  (buffer-major-mode-matcher
   emacs-lisp-mode
   (if (and (buffer-file-name)
            (s-ends-with-p "-tests.el" (buffer-file-name)))
       (lambda () (eval-buffer) (ert t))
     'eval-buffer))

  (lambda ()
    (when (file-exists-p "Makefile") "make"))

  (lambda ()
    (when (equal "pkgdef" (f-ext (buffer-file-name (current-buffer))))
      (format "mpm build --pkgdef_file=%s --alsologtostderr"
              (buffer-file-name (current-buffer)))))

  (buffer-major-mode-matcher
   c-mode
   (let ((fn (f-filename (buffer-file-name)))
         (pipe-in (if (file-exists-p "test.in") " < test.in" ""))
         (speed (if (and (boundp 'c-ofast-compilation) c-ofast-compilation)
                    "-Ofast" "-g")))
     (format "gcc %s -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s"
             speed fn pipe-in)))

  (buffer-major-mode-matcher
   c++-mode
   (let ((fn (f-filename (buffer-file-name)))
         (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
     (format "g++ %s -std=c++11 && ./a.out %s"
             fn pipe-in)))

  (buffer-major-mode-matcher
   go-mode
   "go test")

  (buffer-major-mode-matcher (tex-mode latex-mode) 'latex-compile)

  (buffer-major-mode-matcher
   python-mode
   (format "python %s" (f-filename (buffer-file-name))))

  (buffer-major-mode-matcher
   (git-rebase-mode text-mode)
   (lambda ()
     (save-buffer)
     (with-editor-finish nil)))

  (buffer-major-mode-matcher
   diff-mode
   (lambda ()
     (save-buffer)
     (server-edit)))

  (buffer-major-mode-matcher clojure-mode 'cider-load-buffer)

  (buffer-major-mode-matcher message-mode-map 'message-send-and-exit)

  (buffer-major-mode-matcher org-mode 'org-export-mine)

  (buffer-major-mode-matcher octave-mode
			     (lambda ()
			       (call-interactively
				(if (region-active-p)
				    'octave-send-region
				  'octave-send-buffer))))

  (buffer-major-mode-matcher (html-mode mhtml-mode)
			     (lambda ()
			       (let ((url
				      (->> (buffer-file-name)
					   (sanitize-filename)
					   (format "file://%s"))))
				 (browser-new-tab url))))

  (buffer-major-mode-matcher
   'js-mode
   (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
     (when (s-ends-with-p "-boot.json" filename)
       (format "cf-boot %s -i free-vars.json" filename))))

  (buffer-major-mode-matcher
   'texinfo-mode
   (concat "texi2any ${EMACS_COMPILATION_FILENAME}"
	   " --html"
	   " --no-number-sections"))

  (buffer-major-mode-matcher
   'nginx-mode
   (concat "sudo service nginx restart"))))


(setf compilation-ask-about-save nil)
(global-set-key (kbd "M-c") 'erjoalgo-compile-compile)
(global-set-key (kbd "M-C")
		(lambda (arg) (interactive "P")
		  (call-interactively
		   'erjoalgo-compile-set-cmd arg)
		  (compile compile-command)))
;;(setf compilation-read-command nil)


(global-set-key (kbd "M-,") 'previous-error)
(global-set-key (kbd "M-.") 'next-error)

;;taken from
;;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)
(setf compilation-ask-about-save nil)
(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

;; (add-to-list 'compilation-finish-functions 'cc-goto-first-error)

(defvar compilation-notify-send nil)
(defun compilation-finished-notify (buff finish-description)
  (with-current-buffer compilation-last-buffer
    (set-face-background 'mode-line
                         (if (s-contains-p "abnormally" finish-description)
                             "orange"
                           "sea green")))
  (message "compilation finished")
  (when compilation-notify-send
    (call-process
     "notify-send" nil 0 nil
     (format "compilation: %s" finish-description))
    (let ((current-hour
	   (third (decode-time (current-time)))))
      (unless (or (> current-hour 23)
		  (< current-hour 9))
	'(beeper-beep)))))

(add-to-list 'compilation-finish-functions
	     'compilation-finished-notify)

;;TODO autoload recompile
(require 'compile)
