(defvar erjoalgo-compile-notify-min-compilation-duration 5
  "if non-nil, do not notify of compilation ended if compilation took less than
 ‘erjoalgo-compile-notify-threshold-secs' seconds")

(setf erjoalgo-compile-notify-min-compilation-duration 0)

(defun erjoalgo-compile-post-compile-message (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (when compilation-state
    (message "compilation %s" (s-trim compilation-state))
  (when (and
         ;; no remaining cmds in the pieline
         (null erjoalgo-compile-command-queue)
         ;; this fails when emacs is not raised and therefore not visible...
         ;; (not (frame-visible-p (selected-frame)))
         (not (member (emacs-pid) (stumpwm-visible-window-pids)))
         (or (null erjoalgo-compile-notify-min-compilation-duration)
             (>=
              (- (time-to-seconds) erjoalgo-compile-last-compilation-start-time)
              erjoalgo-compile-notify-min-compilation-duration)))
    (stumpwm-message
     (let ((red "^1") (green "^2"))
       (format "%scompilation %s^*"
               (if (equal "finished" (s-trim compilation-state)) green red)
               compilation-state)))))
)

(add-hook 'erjoalgo-compile-pipeline-finished-hook 'erjoalgo-compile-post-compile-message)

(defun erjoalgo-compile-disable-query-on-proc-exit (proc)
  (set-process-query-on-exit-flag proc nil))

(add-hook 'compilation-start-hook 'erjoalgo-compile-disable-query-on-proc-exit)
