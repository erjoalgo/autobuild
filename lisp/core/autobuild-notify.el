(defvar autobuild-notify-threshold-secs 10
  "Mininum number of seconds that must have elapsed since compilation start time to issue a notification.

  If nil, disable notifications.
  If t, always issue notifications.")

(setf autobuild-notify-threshold-secs t)

(defun autobuild-notify (compilation-buffer compilation-state)
  ;; TODO try notify-send, xmessage, audible/visible beep...
  (safe-wrap
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
             (stumpwm-message msg color))))))))

(add-hook 'compilation-finish-functions 'autobuild-notify)
