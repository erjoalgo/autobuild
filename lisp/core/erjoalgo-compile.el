(defvar erjoalgo-compile-last-compilation-start-time nil
  "The start time in seconds since epoch of the last compilation")

(defvar erjoalgo-compile-command-queue nil
  "Internal. Keep track of the next command in the global compilation pipeline")

(defvar erjoalgo-compile-original-compile-buffer nil
  "Internal. Keep track of the original buffer
where the global compilation pipeline was invoked")

(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defvar erjoalgo-compile-pipeline-finished-hook nil
  "Hook called when the entire compilation pipeline has completed")

(make-variable-buffer-local 'erjoalgo-compilation-next-buffer)

(setf compilation-save-buffers-predicate (lambda () nil))

(defvar-local compile-command-set-interactively nil)
(defvar-local erjoalgo-compilation-next-buffer nil)

(put 'compile-command 'safe-local-variable 'stringp)

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

(defun walk-up-directories (dir)
  (loop with dir = default-directory
	while dir
	collect dir
	do (setf dir (f-dirname dir))))


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
