(require 'cl)
(require 'compile)

(defmacro -> (&rest forms)
  (if (cadr forms)
      (destructuring-bind (first second . rest) forms
        (destructuring-bind (a . a-rest) (if (atom second)
                                             (cons second nil)
                                           second)
          `(-> ,(apply 'list a first a-rest) ,@rest)))
    (car forms)))

(let ()
   (with-current-buffer (find-file-noselect "/usr/local/google/home/ejalfonso/git/autobuild/autobuild.el") (eval-buffer))
   (with-current-buffer (find-file-noselect "/usr/local/google/home/ejalfonso/git/autobuild/autobuild-test.el")
     (eval-buffer)
     (-> #'autobuild-pipeline-nested-test
         autobuild-rule-find
         autobuild-rule-action
         autobuild-run-action)))

(sit-for 10)
