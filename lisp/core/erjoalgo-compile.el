(defun erjoalgo-compile-compile (arg)
  (interactive "P")
  (if (and arg compile-command) (recompile)
    (let* ((cmd-list
	    (or
	     (and compile-command-set compile-command)
	     (erjoalgo-compile-read-file-local-cmd-list);;file-local
	     (erjoalgo-compile-cmd-for-buffer (current-buffer));;matcher
	     (call-interactively 'erjoalgo-compile-ask '(4)));;ask user and save
	    )

	   (cmd-list (if (or (functionp cmd-list)
			     (atom cmd-list))
			 (list cmd-list)
		       cmd-list))
	   (emacs-filename-env-directive
	    (concat "EMACS_COMPILATION_FILENAME=" (buffer-file-name (current-buffer))))
	   (process-environment (append (list emacs-filename-env-directive)
					process-environment))
	   )
      (loop for cmd in cmd-list do
	    (cond
	     ((stringp cmd) (let ((compile-command cmd))
			      (compile cmd)
			      (let ((proc (get-buffer-process "*compilation*")))
				(assert proc)
				(set-process-query-on-exit-flag proc nil))))
	     ((functionp cmd) (funcall cmd))
	     ((null cmd) (error "no compile command found for this buffer"))
	     (t (error "cmd must be function or string, not %s" cmd))))
      (when (and (boundp 'erjoalgo-compilation-next-buffer)
		 erjoalgo-compilation-next-buffer)
	(if (get-buffer erjoalgo-compilation-next-buffer)
	    (progn
	      (message "exit rec edit for %s compilation..."
		       erjoalgo-compilation-next-buffer)
	      (recursive-edit)
	      (switch-to-buffer erjoalgo-compilation-next-buffer)
	      (when compile-command
		(erjoalgo-compile-compile arg)))))
      )))

(defvar-local compile-command-set nil)
(defvar-local erjoalgo-compilation-next-buffer nil)

(defun erjoalgo-compile-read-file-local-cmd-list ()
  ;;(read-file-local-variable-value 'compile-command)
  ;;TODO read file local compile-command
  (cdr (assoc 'compile-command file-local-variables-alist))
  )

(defun erjoalgo-compile-cmd-for-buffer (buffer)
  (loop for matcher in erjoalgo-compile-cmd-for-buffer
	thereis (funcall matcher buffer)))

(defvar erjoalgo-compile-cmd-for-buffer ()
  "list of functions, each should return
the command for compiling a particular buffer,
or nil if unknown")

(defun erjoalgo-compile-ask (cmd)
  (interactive
   (list
    (if current-prefix-arg;; read-lisp-object
	(read--expression "enter compile command sexp: ")
      (read-shell-command "enter compile command: "
			  (and (boundp 'compile-command) compile-command)))))

  (add-file-local-variable 'compile-command cmd)
  (setf compile-command cmd)
  (setf compile-command-set t)
					;(compile compile-command)
  )

(defun erjoalgo-compile-set-next-buffer (next-buffer)
  (interactive "benter next buffer to compile: ")
  ;; (add-file-local-variable 'erjoalgo-compile-next-buffer next-buffer)
  (add-file-local-variable 'erjoalgo-compilation-next-buffer next-buffer)
  (setf erjoalgo-compilation-next-buffer next-buffer))

(defun wrap-ignore-args (fun)
  (lexical-let ((fun fun))
    (lambda (&rest args) (funcall fun))))

(defmacro buffer-major-mode-matcher (modes &rest forms)
  `(lambda (buffer)
     (when (member
	    (buffer-local-value 'major-mode buffer)
	    ',(if (atom modes) (list modes) modes))
       (with-current-buffer buffer ,@forms))))

(defun walk-up-directories (dir)
  (loop with dir = default-directory
	while dir
	collect dir
	do (setf dir (f-dirname dir))))

(setf
 erjoalgo-compile-cmd-for-buffer
 (list
  (buffer-major-mode-matcher
   fundamental-mode
   ;;git commit
   (lambda ()
     (save-buffer)
     (server-edit)))

  (buffer-major-mode-matcher
   sh-mode
   (let ((fn (f-filename (buffer-file-name))))
     (if (file-executable-p fn)
	 (format "./%s" fn)
       (format "bash %s" fn))))

  (buffer-major-mode-matcher
   java-mode
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
		 (when jvm-proxy (concat jvm-proxy " "))))
       )))

  (buffer-major-mode-matcher
   c-mode
   (if (file-exists-p "Makefile")
       "make"
     (let ((fn (f-filename (buffer-file-name)))
	   (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
       (format "gcc -g -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s"
	       fn pipe-in))))

  (lambda (buffer)
    (when (string= (f-base (buffer-file-name buffer))
		   "Makefile") "make"))

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

  (buffer-major-mode-matcher 'emacs-lisp-mode 'eval-buffer)

  (buffer-major-mode-matcher 'lisp-mode 'slime-compile-and-load-file)

  (buffer-major-mode-matcher 'octave-mode
			     (lambda ()
			       (call-interactively
				(if (region-active-p)
				    'octave-send-region
				  'octave-send-buffer))))

  (buffer-major-mode-matcher 'html-mode
			     (lambda ()
			       (let ((url
				      (->> (buffer-file-name)
					   (sanitize-filename)
					   (format "file://%s"))))
				 (firefox-new-tab url))))

  (buffer-major-mode-matcher
   'js-mode
   (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
     (when (s-ends-with-p "-boot.json" filename)
       (format "cf-boot %s -i free-vars.json" filename))))
  ))


(setf compilation-ask-about-save nil)
(global-set-key (kbd "M-c") 'erjoalgo-compile-compile)
(global-set-key (kbd "M-C")
		(lambda (arg) (interactive "P")
		  (call-interactively
		   'erjoalgo-compile-ask arg)
		  (compile compile-command)))
;;(setf compilation-read-command nil)

(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

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

(add-to-list 'compilation-finish-functions 'cc-goto-first-error)

(defvar compilation-notify-send nil)
(defun compilation-finished-notify (buff finish-description)
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
