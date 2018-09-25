(defvar erjoalgo-compile-notify-min-compilation-duration 5
  "if non-nil, notify that compilation ended unless compilation took less than
 ‘erjoalgo-compile-notify-threshold-secs' seconds")

(setf erjoalgo-compile-notify-min-compilation-duration 10)

(defun erjoalgo-compile-post-compile-message (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (safe-wrap
   (when compilation-state
     (let ((msg (format "compilation %s: %s" (s-trim compilation-state)
                        compile-command)))
       (message "%s" msg)
       (when (and
              ;; no remaining cmds in the pieline
              (null erjoalgo-compile-command-queue)
              ;; this fails when emacs is not raised and therefore not visible...
              ;; (not (frame-visible-p (selected-frame)))
              erjoalgo-compile-notify-min-compilation-duration
              (>= (- (time-to-seconds) erjoalgo-compile-last-compilation-start-time)
                  erjoalgo-compile-notify-min-compilation-duration)
              (not (member (emacs-pid) (stumpwm-visible-window-ids t))))
         (stumpwm-message (stumpwm-color
                           (if (equal "finished" (s-trim compilation-state)) 'green
                             'red))))))))

(add-hook 'erjoalgo-compile-pipeline-finished-hook 'erjoalgo-compile-post-compile-message)

(defun erjoalgo-compile-disable-query-on-proc-exit (proc)
  (set-process-query-on-exit-flag proc nil))

(add-hook 'compilation-start-hook 'erjoalgo-compile-disable-query-on-proc-exit)


(defvar compilation-interpret-ansi-color nil)

(setf compilation-interpret-ansi-color t);; todo make buffer-local and mode-local

(defun maybe-colorize-compilation-buffer ()
  ;; (require 'ansi-color)
  ;; https://stackoverflow.com/questions/3072648/
  (when compilation-interpret-ansi-color
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point)))))

(add-hook 'compilation-filter-hook 'maybe-colorize-compilation-buffer)
