(require 'autobuild)

(defvar autobuild-pipeline-test-filename "/tmp/autobuild-test.log")

(defun append-beep-command (beep &optional no-beep)
  (format "echo %s >> %s; %s"
          (shell-quote-argument (number-to-string beep))
          autobuild-pipeline-test-filename
          (unless no-beep
            (format "beep -f %s" beep)))
  (format "beep -f %s" beep))

(autobuild-define-rule autobuild-pipeline-test
  nil
  (autobuild-pipeline
   ((current-buffer) (append-beep-command 440)
    ((current-buffer) (append-beep-command 660))
    ((current-buffer) (append-beep-command 880)))))

(autobuild-define-rule autobuild-pipeline-nested-test
  nil
  (autobuild-pipeline
   ((current-buffer) (autobuild-rule-find #'autobuild-pipeline-test))
   ((current-buffer) (append-beep-command 220))
   ((current-buffer) (autobuild-rule-find #'autobuild-pipeline-test))
   ((current-buffer) (append-beep-command 1200))))

(defmacro -> (&rest forms)
  (if (cadr forms)
      (destructuring-bind (first second . rest) forms
        (destructuring-bind (a . a-rest) (if (atom second)
                                             (cons second nil)
                                           second)
          `(-> ,(apply 'list a first a-rest) ,@rest)))
    (car forms)))

(ert-deftest autobuild-tests-nested-pipeline ()
  (call-process "rm" nil nil nil "-f" autobuild-pipeline-test-filename)
  (-> #'autobuild-pipeline-nested-test
      autobuild-rule-find
      autobuild-rule-action
      autobuild-run-action)
  (let ((contents (with-current-buffer
                      (find-file-noselect autobuild-pipeline-test-filename)
                    (buffer-string)))
        (expected (s-join "\n"
                          '("440" "660" "880"
                            "220"
                            "440" "660" "880"
                            "1200"
                            ""))))
    (should (equal contents expected))))
