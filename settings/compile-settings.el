(defun compile-set-command-and-run (cmd)
  (interactive (list (read-shell-command "enter compile command: " compile-command)))
  (setf compile-command cmd)
  (add-file-local-variable 'compile-command compile-command)
  (compile compile-command))


  
(global-set-key (kbd "M-c") 'compile)
(global-set-key (kbd "M-C") 'compile-set-command-and-run)
(setf
 ;compilation-read-command nil
 compilation-ask-about-save nil )



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
;- See more at: http://compgroups.net/comp.emacs/show-tail-of-compilation-buffer-by-auto-scrolling/111626#sthash.1r8ETB0J.dpuf
