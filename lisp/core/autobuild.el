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
(unless (require 'selcand nil t)
  (defun selcand-select (cands &optional prompt stringify)
    "Use PROMPT to prompt for a selection from CANDS candidates."
    (let* ((stringify (or stringify #'prin1-to-string))
           (prompt (or prompt "select candidate: "))
           (hints-cands
            (cl-loop for cand in cands
                     as string = (funcall stringify cand)
                     collect (cons string cand)))
           (choice (minibuffer-with-setup-hook
                       #'minibuffer-completion-help
                     (completing-read prompt (mapcar #'car hints-cands)
                                      nil
                                      t)))
           (cand (alist-get choice hints-cands nil nil #'equal)))
      cand)))

(cl-defstruct autobuild-rule
  major-modes
  nice
  genaction)

(defvar autobuild-rules-alist nil)

(defun autobuild-add-rule (name rule)
  "Internal.  Add a rule RULE with NAME as the key."
  (setf (alist-get name autobuild-rules-alist) rule))

(defalias 'autobuild-nice #'ignore)

;;;###autoload
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

;;;###autoload
(defmacro autobuild-pipeline (&rest buffer-rule-list)
  "Define a build pipeline.

  Each entry in BUFFER-RULE-LIST has the form (BUFFER RULE),
  where BUFFER is the next buffer in the pipeline, and RULE
  is the rule to invoke within BUFFER to generate an action.

  If ACTION is either a (compile command) string or a function that
  returns a compilation buffer,compilation is executed asynchronously
  and the pipeline is resumed upon compilation finish.  Otherwise, ACTION
  is executed synchronously.

  If any step in the compilation pipeline fails, via either an error or
  an abnormal compilation finish state, any remaining steps in the pipeline
  are aborted."

  `(lambda ()
     (autobuild-pipeline-run
      (list ,@(cl-loop for (buffer rule-name) in buffer-rule-list
                       collect `(list ,buffer ,rule-name))))))

(defun autobuild-rule-action (rule)
  "Generate an action for rule RULE."
  (let ((original-buffer (current-buffer)))
    (prog1
        (funcall (autobuild-rule-genaction rule))
      (unless (eq (current-buffer) original-buffer)
        (error "‘genaction' of rule %s should not change buffers or have side effects"
               rule)))))

(defun autobuild-pipeline-run (rules-remaining)
  "Run the RULES-REMAINING of an autobuild pipeline.  See ‘autobuild-pipeline'."
  (when rules-remaining
    (cl-destructuring-bind (buffer name) (car rules-remaining)
      (with-current-buffer buffer
        (let* ((rule (alist-get name autobuild-rules-alist))
               (action (autobuild-rule-action rule)))
          (unless action
            (error "Rule %s in pipeline did not generate an action" rule))
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
  (string-match-p ".*abnormally.*" compilation-finished-message))

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
           (let ((major-modes (autobuild-rule-major-modes rule)))
             (and
              (or
               (eq t major-modes)
               (if (atom major-modes)
                   (eq major-mode major-modes)
                 (cl-find major-mode major-modes)))
              (autobuild-rule-action rule)))
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

;;;###autoload
(defun autobuild-build (&optional prompt)
  "Build the current buffer.

   If PROMPT is non-nil or if there is no known last rule for
    the current buffer,
   prompt for selection of one of the currently-applicable build rules.
   Otherwise, chose the last-executed build rule, if known,
   or the rule with the lowest NICE property (highest priority)."

  (interactive "P")
  (let* ((cands (or (and (not prompt)
                         autobuild-last-rule-name
                         (let* ((last-rule (alist-get autobuild-last-rule-name
                                                      autobuild-rules-alist))
                                action)
                           (if (null last-rule)
                               (progn (warn "rule no longer exists: %s" autobuild-last-rule-name)
                                      nil)
                             (when (setq action
                                         (funcall (autobuild-rule-genaction last-rule)))
                               (list (list autobuild-last-rule-name
                                           last-rule
                                           action))))))
                    (autobuild-current-build-actions)))
         (choice (cond ((null cands) (error "No build rules matched"))
                       ((and (not prompt) (null (cdr cands))) (car cands))
                       (t (selcand-select cands "select build rule: "
                                          ;; TODO sort vertically
                                          (lambda (name-rule-action)
                                            (format "%s (%s)"
                                                    (car name-rule-action)
                                                    (autobuild-rule-nice
                                                     (cadr name-rule-action)))))))))
    (cl-assert choice)
    (setq autobuild-last-rule-name (car choice))
    (autobuild-run-action (caddr choice))))

(defun autobuild-run-action (action)
  "Execute a rule-generated ACTION as specified in ‘autobuild-define-rule'."
  (cl-assert action)
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autobuild.el ends here
