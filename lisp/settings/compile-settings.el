(defun erjoalgo-compile-disable-query-on-proc-exit (proc)
  (set-process-query-on-exit-flag proc nil))

(add-hook 'compilation-start-hook 'erjoalgo-compile-disable-query-on-proc-exit)

(defvar compilation-interpret-ansi-color nil)

(setf compilation-interpret-ansi-color t);; todo make buffer-local and mode-local

(setf compilation-save-buffers-predicate (lambda () nil))

;;taken from
;;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)

(setf compilation-ask-about-save nil)

(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

(defun maybe-colorize-compilation-buffer ()
  ;; (require 'ansi-color)
  ;; https://stackoverflow.com/questions/3072648/
  (when compilation-interpret-ansi-color
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(add-hook 'compilation-filter-hook 'maybe-colorize-compilation-buffer)

(defadvice next-error-find-buffer (around prioritize-compilation-buffer activate)
  (setq ad-return-value
        (if (and (bound-and-true-p autobuild-last-compilation-buffer)
                 (buffer-live-p autobuild-last-compilation-buffer))
            (progn
              autobuild-last-compilation-buffer)
          ad-do-it)))
