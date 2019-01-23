(defvar autobuild-notify-threshold-secs 10
  "Mininum number of seconds that must have elapsed since compilation-start to issue a notification.

If nil, disable notifications.
If t, always issue notifications.")

(setf autobuild-notify-threshold-secs t)

(defun autobuild-notify (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  ;; (safe-wrap
  (when compilation-state
    (with-current-buffer compilation-buffer
      (let ((msg (format "compilation %s: %s"
                         (s-trim compilation-state) compile-command)))
        (message "%s" msg)
        (when (and
               ;; this fails when emacs is not raised and therefore not visible...
               ;; (not (frame-visible-p (selected-frame)))
               autobuild-notify-threshold-secs
               (or (eq autobuild-notify-threshold-secs t)
                   (>= (- (time-to-seconds)
                          autobuild-compilation-start-time)
                       autobuild-notify-threshold-secs))
               (not (member (emacs-pid) (stumpwm-visible-window-ids t))))
          (let ((color (if (equal "finished" (s-trim compilation-state))
                           'green 'red)))
            (stumpwm-message msg color)))))))

(add-hook 'compilation-finish-functions 'autobuild-notify)

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
