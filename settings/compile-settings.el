(defun compile-set-command-and-run (cmd)
  (interactive (list (read-shell-command "enter compile command: " compile-command)))
  (setf compile-command cmd)
  (add-file-local-variable 'compile-command compile-command)
  (compile compile-command))


  
(global-set-key (kbd "M-c") 'compile)
(global-set-key (kbd "M-C") 'compile-set-command-and-run)
(setf compilation-read-command nil
      compilation-ask-about-save nil )



(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "M-n") 'next-error)

;(add-hook 'go-mode-hook (lambda () (setf compile-command "go run *.go")))
(add-hook 'go-mode-hook (lambda () (setf compile-command "go test")))
