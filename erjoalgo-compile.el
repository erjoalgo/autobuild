(defun compilation-exited-abnormally-p (compilation-finished-message)
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defvar erjoalgo-compile-pipeline-finished-hook nil
  "Hook called when the entire compilation pipeline has completed")

(make-variable-buffer-local 'erjoalgo-compilation-next-buffer)

(defvar-local erjoalgo-compilation-next-buffer nil)

(defun erjoalgo-compile-set-next-buffer (next-buffer)
  (interactive "benter next buffer to compile: ")
  ;; (add-file-local-variable 'erjoalgo-compile-next-buffer next-buffer)
  (add-file-local-variable 'erjoalgo-compilation-next-buffer next-buffer)
  (setf erjoalgo-compilation-next-buffer next-buffer))
