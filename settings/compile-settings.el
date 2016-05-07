(defun compile-set-command-and-run (cmd)
  (interactive (list (read-shell-command "enter compile command: " compile-command)))
  (setf compile-command cmd)
  (add-file-local-variable 'compile-command compile-command)
  (compile compile-command))


  
;(global-set-key (kbd "M-c") 'compile)
(global-set-key (kbd "M-c") 'recompile)
(global-set-key (kbd "M-C") 'compile-set-command-and-run)
(setf compilation-ask-about-save nil )
(setf compilation-read-command t)



(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

;(add-hook 'go-mode-hook (lambda () (setf compile-command "go run *.go")))
(add-hook 'go-mode-hook (lambda () (setf compile-command "go test")))


;taken from
;http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626
(setq compilation-scroll-output t)

(defun cc-goto-first-error (buffer exit-condition)
  (with-current-buffer buffer
    (goto-char (point-min))
    (compilation-next-error 1)))

(add-to-list 'compilation-finish-functions 'cc-goto-first-error)

(defun compilation-finished-notify (buff finish-description)
  (call-process
   "notify-send" nil 0 nil
   (format "compilation: %s" finish-description))
  (let ((current-hour
	 (third (decode-time (current-time)))))
    (unless (or (> current-hour 23)
	      (< current-hour 9))
      ;;this theme is nice. text easy to read, dark background
      ;;only load at night?
      '(beeper-beep)
      )))

(add-to-list 'compilation-finish-functions
	     'compilation-finished-notify)
