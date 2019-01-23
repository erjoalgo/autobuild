(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defvar erjoalgo-compile-pipeline-finished-hook nil
  "Hook called when the entire compilation pipeline has completed")

(make-variable-buffer-local 'erjoalgo-compilation-next-buffer)

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

;;TODO autoload recompile
(require 'compile)
