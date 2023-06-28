(require 'autobuild)
(require 'cl-lib)

(autobuild-mode t)

(ert-deftest autobuild-test-mode-filter ()
  (let (autobuild-rules-list)
    (eval
     '(autobuild-define-rule test-sh-mode (sh-mode)
        (autobuild-nice 7)
        "bash"))
    (with-temp-buffer
      (should (eq 1 (length autobuild-rules-list)))
      (should (eq 0 (length (autobuild-applicable-rule-actions))))
      (sh-mode)
      (should (eq 1 (length (autobuild-applicable-rule-actions)))))))

(defun make-sh-file (identifier contents)
  (with-current-buffer
      (find-file-noselect
       (make-temp-file (format "autobuild-test-%s-" identifier) nil ".sh"))
    (insert contents)
    (save-buffer)
    (current-buffer)))

(ert-deftest autobuild-test-notification ()
  (let* (autobuild-rules-list
         finish-order
         (autobuild-notification-function
          (lambda (buffer state)
            (add-to-list 'finish-order compile-command t)))
         (autobuild-notify-threshold-secs t)
         (compilation-auto-rename-buffer-fn
          (lambda (&rest args)
             (with-current-buffer "*compilation*"
               (rename-buffer (generate-new-buffer-name "*autobuild-test-*")))))
         (compilation-start-hook
          (cons compilation-auto-rename-buffer-fn compilation-start-hook)))

    (progn
      (should (null finish-order))
      (funcall autobuild-notification-function 'buffer 'state)
      (should (eq 1 (length finish-order)))
      (pop finish-order))

    (eval
     `(autobuild-define-rule test-sh-mode (sh-mode)
        (autobuild-nice 7)
        (format "bash %s" buffer-file-name)))

    (let* ((fast (make-sh-file "fast" "sleep 1"))
           (slow (make-sh-file "slow" "sleep 4"))
           (first (make-sh-file "first" "true"))
           (run-order (list fast slow first))
           (finish-order-expected (list first fast slow)))
      (cl-loop for buf in run-order do
               (with-current-buffer buf
                 (should (eq 1 (length (autobuild-applicable-rule-actions))))
                 (autobuild-build nil)))
      (while (< (length finish-order) (length run-order)) (sit-for 1))
      (let ((expected (mapcar
                       (lambda (buffer)
                         (format "bash %s" (buffer-file-name buffer)))
                       finish-order-expected)))
        (should (equal finish-order expected))))))


(ert-deftest autobuild-test-last-rule ()
  (let (autobuild-rules-list
        ran)
    (eval
     '(autobuild-define-rule low-nice (sh-mode)
        (autobuild-nice 7)
        (lambda () (setq ran 'low))))
    (eval
     '(autobuild-define-rule high-nice (sh-mode)
        (autobuild-nice 10)
        (lambda () (setq ran 'high))))
    (with-temp-buffer
      (sh-mode)
      (should (eq 2 (length (autobuild-applicable-rule-actions))))
      (should (null ran))
      (should (null autobuild-last-local-invocation))
      (autobuild-build nil)
      (should (eq 'low ran))
      (should (eq (autobuild--invocation-rule autobuild-last-local-invocation) #'low-nice))
      (cl-letf (((symbol-function #'autobuild-candidate-select)
              (lambda (cands _prompt _stringify-fn)
                (cl-loop for action in cands
                         thereis (when (eq (autobuild--invocation-rule action)
                                           #'high-nice)
                                   action)))))
        (autobuild-build t)
        (should (eq 'high ran))
        (should (eq (autobuild--invocation-rule autobuild-last-local-invocation) #'high-nice))
        (autobuild-build nil)
        (should (eq 'high ran))
        (should (eq (autobuild--invocation-rule autobuild-last-local-invocation) #'high-nice))))))

(ert-deftest autobuild-test-prioritizing-rules-defined-first ()
  (let (autobuild-rules-list rule-executed)
    (eval
     '(progn
        (autobuild-define-rule older-rule (sh-mode)
          (autobuild-nice 7)
          (lambda () (setq rule-executed 'older)))
        (autobuild-define-rule newer-rule (sh-mode)
          (autobuild-nice 7)
          (lambda () (setq rule-executed 'newer)))))
    (should (eq 'newer-rule (car autobuild-rules-list)))
    (with-temp-buffer
      (sh-mode)
      (should (eq 2 (length (autobuild-applicable-rule-actions))))
      (should (eq 'older-rule
                  (autobuild--invocation-rule
                   (car (autobuild-applicable-rule-actions)))))
      (autobuild-build nil)
      (should (eq (autobuild--invocation-rule
                   autobuild-last-local-invocation)
                  'older-rule))
      (should (eq rule-executed 'older)))))
