;;; autobuild.el --- Define and execute build rules and compilation pipelines
;;
;; Filename: autobuild.el
;; Description: Define and execute composable build rules and compilation pipelines
;; Author: Ernesto Alfonso
;; Maintainer: (concat "erjoalgo" "@" "gmail" ".com")

;; Created: Wed Jan 23 20:45:01 2019 (-0800)
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.3"))
;; URL: http://github.com/erjoalgo/autobuild
;; Keywords: compile, build, pipeline, autobuild
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; A framework for defining and executing composable build rules and
;; synchronous or asynchronous compilation pipelines.
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'cl-lib)
(require 'selcand)
(require 'cl-macs)
;; (require 'eieio)

(cl-defstruct autobuild-rule
  ;; name
  major-modes
  nice
  genaction)

(defvar autobuild-rules-alist nil)
;; (setq autobuild-rules-alist nil)

(defun autobuild-add-rule (name rule)
  "Internal.  Add a rule RULE with NAME as the key."
  (setf (alist-get name autobuild-rules-alist) rule))

(defalias 'autobuild-nice #'ignore)

(cl-defmacro autobuild-define-rule (name
                                    major-modes
                                    &rest body)
  "Define a build rule NAME.

   When ‘major-mode' is in MAJOR-MODES, or when MAJOR-MODES is t,
   the action-generator BODY is evaluated, which returns an action
   which must be one of the following types:

   nil if the generator doesn't know how to generate an action.
   string is interpreted as a compile-command, which is executed via ‘compile'
   function is executed via ‘funcall'"
  (cl-assert major-modes)
  (when (and (not (eq t major-modes))
             (member t major-modes))
    (error "Invalid major mode specification"))
  (let* ((autobuild-directives '(autobuild-nice))
         (directives
          (cl-loop for top-level-form in body
                ;; TODO remove directives from body
               when (and (listp top-level-form)
                         (member (car top-level-form) autobuild-directives))
               collect (cons (car top-level-form)
                             (cadr top-level-form))))
        (nice (or (alist-get 'autobuild-nice directives) 10)))
    `(autobuild-add-rule
      ',name
      (make-autobuild-rule
       ;; :name ',name
       :major-modes ',major-modes
       :nice ,nice
       :genaction (defun ,name () ,@body)))))


(defvar-local autobuild-rules-remaining nil)

(defmacro autobuild-pipeline (&rest buffer-rule-list)
  "Define a build pipeline.

  Each entry in BUFFER-RULE-LIST has the form (BUFFER RULE),
  where BUFFER is the next buffer in the pipeline, and RULE
  is the rule to invoke within BUFFER to generate an action.

  If ACTION is either a (compile command) string or a compilation buffer,
  compilation is executed asynchronously and the pipeline is resumed
  upon compilation finish.  Otherwise, ACTION is executed synchronously.

  If any step in the compilation pipeline fails, via either an error or
  an abnormal compilation finish state, any remaining steps in the pipeline
  are aborted."

  `(lambda ()
     (autobuild-pipeline-run
      (list ,@(cl-loop for (buffer rule-name) in buffer-rule-list
                       collect `(list ,buffer ,rule-name))))))

(defun autobuild-pipeline-run (rules-remaining)
  "Run the RULES-REMAINING of an autobuild pipeline.  See ‘autobuild-pipeline'."
  (when rules-remaining
    (cl-destructuring-bind (buffer name) (car rules-remaining)
      (with-current-buffer buffer
        (let* ((rule (alist-get name autobuild-rules-alist))
               (action (funcall (autobuild-rule-genaction rule))))
          (assert action)
          (let ((result (autobuild-run-action action)))
            (if (and (bufferp result)
                     (eq 'compilation-mode (buffer-local-value 'major-mode result)))
                (with-current-buffer result
                  (setq autobuild-rules-remaining (cdr rules-remaining))
                  (message "scheduling remaining rules: %s" autobuild-rules-remaining))
              (progn
                ;; TODO fail early on non-zero exit, error
                ;; or ensure each action errs
                (setq autobuild-rules-remaining (cdr rules-remaining))
                (autobuild-pipeline-run (cdr rules-remaining))))))))))


(defun autobuild-compilation-exited-abnormally-p (compilation-finished-message)
  "Determine from COMPILATION-FINISHED-MESSAGE whether compilation failed."
  (s-contains-p "abnormally" (s-trim compilation-finished-message)))

(defun autobuild-pipeline-continue (compilation-buffer finish-state)
  "Internal.  Used to resume an asynchronous pipeline.

   COMPILATION-BUFFER FINISH-STATE are the arguments passed
   to functions in ‘compilation-finish-functions'."
  (with-current-buffer compilation-buffer
    (when (bound-and-true-p autobuild-rules-remaining)
      (if (autobuild-compilation-exited-abnormally-p finish-state)
          (progn
            (message "aborting pipeline: %s" autobuild-rules-remaining)
            (setq autobuild-rules-remaining nil))
        (progn
          (message "continuing with pipeline: %s" autobuild-rules-remaining)
          (autobuild-pipeline-run autobuild-rules-remaining))))))

(add-hook 'compilation-finish-functions #'autobuild-pipeline-continue)

(defun autobuild-current-build-actions ()
  "Return a list of the currently applicable build actions.

   A rule RULE is applicable if the current major mode is contained in the
   rule's list of major modes, and if the rule generates a non-nil action."

  (cl-loop for (name . rule) in (reverse autobuild-rules-alist)
           as action =
           (let ((major-modes (autobuild-rule-major-modes rule))
                 (genaction (autobuild-rule-genaction rule)))
             (and
              (or
               (eq t major-modes)
               (if (atom major-modes)
                   (eq major-mode major-modes)
                 (find major-mode major-modes)))
              (funcall genaction)))
           when action
           collect (list name rule action) into cands
           finally (return (autobuild-sort-by (lambda (rule-action)
                                      (autobuild-rule-nice (cadr rule-action)))
                                    cands))))

(global-set-key (kbd "M-c") #'autobuild-build)

(defvar-local autobuild-last-rule-name nil)

(defun autobuild-sort-by (key list)
  "Sort LIST by the key-function KEY."
  (sort list (lambda (a b) (< (funcall key a) (funcall key b)))))

(defun autobuild-build (&optional prompt)
  "Build the current buffer.

   If PROMPT is non-nil or if there is no known last rule for
    the current buffer,
   prompt for selection of one of the currently-applicable build rules.
   Otherwise, chose the last-executed build rule, if known,
   or the rule with the lowest NICE property (highest priority)."

  (interactive "P")
  (let* ((cands (or (and prompt
                         autobuild-last-rule-name
                         (let* ((last-rule (alist-get autobuild-last-rule-name
                                                      autobuild-rules-alist))
                                (action (funcall (autobuild-rule-genaction last-rule))))
                           (when action
                             (list (list autobuild-last-rule-name
                                         last-rule
                                         action)))))
                    (autobuild-current-build-actions)))
         (choice (cond ((null cands) (error "No build rules matched"))
                       ((null (cdr cands)) (car cands))
                       (t (selcand-select cands "select build rule: "
                                          ;; TODO sort vertically
                                          (lambda (name-rule-action)
                                            (format "%s (%s)"
                                                    (car name-rule-action)
                                                    (autobuild-rule-nice
                                                     (cadr name-rule-action)))))))))
    (assert choice)
    (setq autobuild-last-rule-name choice)
    (autobuild-run-action (caddr choice))))

(defun autobuild-run-action (action)
  "Execute a rule-generated ACTION as specified in ‘autobuild-define-rule'."
  (assert action)
  (cond
   ((stringp action) (autobuild-run-string-command action))
   ((commandp action) (call-interactively action))
   ((functionp action) (funcall action))
   (t (error "Action must be string or function, not %s" action))))

(defvar-local autobuild-compilation-start-time nil)

(defvar-local autobuild-last-compilation-buffer nil)

(defun autobuild-run-string-command (cmd)
  "Execute CMD as an asynchronous command via ‘compile'."
  (let ((emacs-filename-env-directive
         ;; allow file-local compile commands to use rename-proof filename
         (concat "AUTOBUILD_FILENAME=" (buffer-file-name (current-buffer)))))
    (push emacs-filename-env-directive process-environment)
    ;; TODO decouple this from autobuild
    (let* ((ansi-color-for-comint-mode t))
      (compile cmd))))

(defun autobuild-compilation-buffer-setup (compilation-buffer &optional
                                                              original-buffer cmd)
  "Add information needed by autobuild on a new compilation.

   COMPILATION-BUFFER points to the newly started compilation buffer,
   ORIGINAL-BUFFER points to the buffer where the compilation originated, and
   CMD should be the compilation command."
  (when original-buffer
    (with-current-buffer original-buffer
      (setq autobuild-last-compilation-buffer compilation-buffer)))
  (with-current-buffer compilation-buffer
    ;; TODO check if this is already available in compile
    (setq autobuild-compilation-start-time (time-to-seconds)
          compile-command (or compile-command cmd))))

(defadvice compilation-start (after
                              autobuild-compilation-buffer-setup-advice
                              activate)
  "Around advice to invoke ‘autobuild-compilation-buffer-setup' on a new compilation."
  (autobuild-compilation-buffer-setup
   ad-return-value
   (current-buffer)
   (ad-get-arg 1)))

(provide 'autobuild)

;; default rules and examples
(autobuild-define-rule
 autobuild-file-local-compile-command
 t
 "A rule that matches any buffer whose file-local compile-command is set"
 (autobuild-nice 9)
 (cdr (assoc 'compile-command file-local-variables-alist)))

(autobuild-define-rule
 autobuild-git-commit
 (fundamental-mode)
 (save-buffer)
 (server-edit))

(autobuild-define-rule
 autobuild-run-executable
 t
 (autobuild-nice 9)
 (when (buffer-file-name)
   (let ((fn (f-filename (buffer-file-name))))
     (if (and fn (file-executable-p fn))
         (format "./%s" fn)))))

(autobuild-define-rule autobuild-dired-build-file-at-point
                       (dired-mode)
                       (when (dired-file-name-at-point)
                         (lambda ()
                           (with-temporary-current-file
                            (dired-file-name-at-point)
                            (call-interactively #'autobuild-build)))))

(autobuild-define-rule autobuild-shell-script
                       (sh-mode)
                       (let ((fn (f-filename (buffer-file-name))))
                         (format "bash %s" fn)))

(autobuild-define-rule autobuild-shell-script-syntax-check
                       (sh-mode)
                       (autobuild-nice 15)
                       (let ((fn (f-filename (buffer-file-name))))
                         (format "bash -n %s" fn)))

(autobuild-define-rule autobuild-java-mode
                       (java-mode nxml-mode)
                       (when (or (eq 'java-mode major-mode)
                                 (and (buffer-file-name)
                                      (equal (f-filename (buffer-file-name)) "pom.xml")))

                         (let ((f-no-ext
                                (-> (buffer-file-name) (f-filename) (f-no-ext)))
                               (pom-directory (cl-loop with dir =  default-directory
                                                       thereis (and
                                                                (member "pom.xml"
                                                                        (directory-files dir))
                                                                dir)
                                                       while (setq dir (f-dirname dir)))))
                           (if (not pom-directory)
                               (format "javac %s.java && java %s" f-no-ext f-no-ext)
                             (concat "cd " pom-directory " && mvn "
                                     ;;maybe add offline flag
                                     (when (and (boundp 'mvn-offline-p) mvn-offline-p) "-o ")
                                     ;;always clean
                                     "clean "
                                     ;; verify or install
                                     (cond
                                      ((s-ends-with-p "IT" f-no-ext) "verify ")
                                      (t "install "))
                                     ;;maybe add -s *_settings.xml
                                     (let* ((mvn-settings (remove-if-not
                                                           (lambda (filename)
                                                             (s-ends-with-p "settings.xml" filename))
                                                           (directory-files pom-directory)))
                                            (mvn-settings (car mvn-settings)))
                                       (when mvn-settings (concat "-s " mvn-settings " ")))
                                     ;;maybe add proxy opts
                                     (let ((jvm-proxy (let ((https (cdr (assoc "https" url-proxy-services))))
                                                        (if (and https (s-contains? ":" https))
                                                            (apply 'format "-Dhttps.proxyHost=%s -Dhttps.proxyPort=%s"
                                                                   (split-string https ":" t))
                                                          ""))))
                                       (when jvm-proxy (concat jvm-proxy " ")))

                                     (when (and (boundp 'mvn-extra-args)
                                                mvn-extra-args) (concat mvn-extra-args " ")))))))

(autobuild-define-rule autobuild-cl-slime-eval (lisp-mode)
                       #'slime-compile-and-load-file)

(autobuild-define-rule autobuild-cl-asdf (lisp-mode)
                       (let ((filename (f-filename (buffer-file-name))))
                         (when (member (f-ext filename) '("asd" "asdf"))
                           (format "sbcl --load %s --eval \"(ql:quickload '%s)\""
                                   filename
                                   (f-base filename)))))


(autobuild-define-rule autobuild-cl (lisp-mode) 'slime-compile-and-load-file)

(autobuild-define-rule autobuild-el-eval-buffer
 (emacs-lisp-mode)
 (if (and (buffer-file-name)
          (s-ends-with-p "-tests.el" (buffer-file-name)))
     (lambda () (eval-buffer) (ert t))
   #'eval-buffer))

(autobuild-define-rule autobuild-makefile
                       t
                       (when (file-exists-p "Makefile") "make"))

(autobuild-define-rule autobuild-mpm t
                       (when (and (buffer-file-name)
                                  (equal "pkgdef" (f-ext (buffer-file-name))))
                         (format "mpm build --pkgdef_file=%s --alsologtostderr"
                                 (buffer-file-name (current-buffer)))))

(autobuild-define-rule
 autobuild-c
 (c-mode)
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" ""))
       (speed (if (and (boundp 'c-ofast-compilation) c-ofast-compilation)
                  "-Ofast" "-g")))
   (format "gcc %s -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s"
           speed fn pipe-in)))

(autobuild-define-rule autobuild-c++
 (c++-mode)
 (let ((fn (f-filename (buffer-file-name)))
       (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
   (format "g++ %s -std=c++11 && ./a.out %s"
           fn pipe-in)))

(autobuild-define-rule
 autobuild-go
 (go-mode)
 "go test")

(autobuild-define-rule autobuild-latex
                       (tex-mode latex-mode)
                       'latex-compile)

(autobuild-define-rule
 autobuild-python-run
 (python-mode)
 (format "python %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-git-finish
 (git-rebase-mode text-mode)
 (progn
   (save-buffer)
   (with-editor-finish nil)))

(autobuild-define-rule autobuild-diff
 (diff-mode)
 (progn (save-buffer)
        (server-edit)))

(autobuild-define-rule autobuild-clojure (clojure-mode) 'cider-load-buffer)

(autobuild-define-rule autobuild-send-message
                       (message-mode)
                       'message-send-and-exit)

(autobuild-define-rule autobuild-org-export
                       (org-mode) 'org-export-mine)

(autobuild-define-rule autobuild-octave-eval
                       (octave-mode)
                       (call-interactively
                        (if (region-active-p)
                            'octave-send-region
                          'octave-send-buffer)))

(autobuild-define-rule autobuild-html-browse
                       (html-mode mhtml-mode)
                       (let ((url
                              (->> (buffer-file-name)
                                   (sanitize-filename)
                                   (format "file://%s"))))
                         (apply-partially #'browser-new-tab url)))

(autobuild-define-rule autobuild-node-run
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (format "node %s" filename)))

(autobuild-define-rule autobuild-cfboot
 (js-mode)
 (let ((filename (-> (f-filename (buffer-file-name)) sanitize-filename)))
   (when (s-ends-with-p "-boot.json" filename)
     (format "cf-boot %s -i free-vars.json" filename))))

(autobuild-define-rule autobuild-texinfo-build
 (texinfo-mode)
 (concat "texi2any ${EMACS_COMPILATION_FILENAME}"
         " --html"
         " --no-number-sections"))

(autobuild-define-rule autobuild-nginx-restart
 (nginx-mode)
 (concat "sudo service nginx restart"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autobuild.el ends here
