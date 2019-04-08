;;; autobuild.el --- Define and execute build rules and compilation pipelines
;;
;; Filename: autobuild.el
;; Description: Define and execute composable build rules and compilation pipelines
;; Author: Ernesto Alfonso
;; Maintainer: (concat "erjoalgo" "@" "gmail" ".com")

;; Created: Wed Jan 23 20:45:01 2019 (-0800)
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.3") (emacs "25"))
;; URL: http://github.com/erjoalgo/autobuild
;; Keywords: compile, build, pipeline, autobuild, extensions, processes, tools
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
(eval-when-compile (require 'subr-x))

(require 'selcand nil t)
(unless (fboundp #'selcand-select)
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

(defvar autobuild-rules-list nil "A list of all known autobuild rules.")

(defun autobuild-rule-p (rule)
  "Return non-nil if RULE has been registered as an autobuild rule."
  (and (functionp rule)
       (cl-find rule autobuild-rules-list)))

(defvar autobuild-nice 10
  "Dynamic var which the currently executing rule may setq when generating an action.")

(defconst autobuild-nice-default 10
  "Default nice value for rules which do not setq autobuild-nice explicitly.")

(defun autobuild-nice (nice)
  "A wrapper for a rule to set the current action's NICE value."
  (setq autobuild-nice nice))

;;;###autoload
(cl-defmacro autobuild-define-rule (name mode-filter &rest body)
  "Define a build rule NAME.

   When ‘major-mode' is in MODE-FILTER, or when MODE-FILTER is nil,
   the action-generator BODY is evaluated, which returns an action
   which must be one of the following types:

   nil if the generator doesn't know how to generate an action.
   string is interpreted as a compile-command, which is executed via ‘compile'
   function is executed via ‘funcall'"
  (declare (indent defun))
  (unless (listp mode-filter)
    (error "Invalid major mode specification"))
  `(progn
     (defun ,name ()
       (when (autobuild-mode-filer-applicable-p ',mode-filter)
         ,@body))
     (pushnew ',name autobuild-rules-list)))

(defvar-local autobuild-pipeline-rules-remaining nil)

;;;###autoload
(defmacro autobuild-pipeline (&rest buffer-rule-list)
  "Define a build pipeline.

  Each entry in BUFFER-RULE-LIST has the form (BUFFER RULE),
  where BUFFER is the next buffer in the pipeline, and RULE
  is the rule to invoke within BUFFER to generate an action.

  If ACTION is either a (compile command) string or a function that
  returns a compilation buffer, compilation is executed asynchronously
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
  "Funcall wrapper to safely obtain an action for rule RULE."
  (assert (functionp rule))
  (let ((original-buffer (current-buffer)))
    (prog1
        (condition-case ex (funcall rule)
          (error
           (error "Error while generating action for rule %s: %s" rule ex)))
      (unless (eq (current-buffer) original-buffer)
        (error "‘genaction' of rule %s should not change buffers or have side effects"
               rule)))))

;; TODO(ejalfonso) fix nested pipeline clobbering remaining rules
;; TODO(ejalfonso) support supressing intermediate pipeline step notifications

(defun autobuild-pipeline-run (rules-remaining)
  "Run the RULES-REMAINING of an autobuild pipeline.  See ‘autobuild-pipeline'."
  (when rules-remaining
    (cl-destructuring-bind (buffer rule-or-action) (car rules-remaining)
      (unless rule-or-action
        ;; TODO use dynamic var to get name of pipeline
        (error "Null rule in piepine"))
      (unless buffer (setq buffer (current-buffer)))
      (with-current-buffer buffer
        (let* ((action (if (autobuild-rule-p rule-or-action)
                           (autobuild-rule-action rule-or-action)
                         rule-or-action)))
          (unless action
            (error "Rule %s in pipeline should have generated an action" rule-or-action))
          (let ((result (autobuild-run-action action)))
            (if (and (bufferp result)
                     (eq 'compilation-mode (buffer-local-value 'major-mode result)))
                (with-current-buffer result
                  (setq autobuild-pipeline-rules-remaining (cdr rules-remaining))
                  (message "scheduling remaining rules: %s" autobuild-pipeline-rules-remaining))
              (progn
                ;; TODO fail early on non-zero exit, error
                ;; or ensure each action errs
                (setq autobuild-pipeline-rules-remaining (cdr rules-remaining))
                (autobuild-pipeline-run (cdr rules-remaining))))))))))

;; TODO
(defvar autobuild-pipeline-finish-hook nil
  "Hook called when the entire pipeline has finished.")

(defun autobuild-compilation-succeeded-p (compilation-finished-message)
  "Determine from COMPILATION-FINISHED-MESSAGE whether compilation failed."
  (equal "finished\n" compilation-finished-message))

(defun autobuild-pipeline-continue (compilation-buffer finish-state)
  "Internal.  Used to resume an asynchronous pipeline.

   COMPILATION-BUFFER FINISH-STATE are the arguments passed
   to functions in ‘compilation-finish-functions'."
  (with-current-buffer compilation-buffer
    (when (bound-and-true-p autobuild-pipeline-rules-remaining)
      (if (not (autobuild-compilation-succeeded-p finish-state))
          (progn
            (message "aborting pipeline: %s" autobuild-pipeline-rules-remaining)
            (setq autobuild-pipeline-rules-remaining nil))
        (progn
          (message "continuing with pipeline: %s" autobuild-pipeline-rules-remaining)
          (autobuild-pipeline-run autobuild-pipeline-rules-remaining))))))

(add-hook 'compilation-finish-functions #'autobuild-pipeline-continue)

(defun autobuild-mode-filer-applicable-p (mode-filter)
  "Determine whether mode-filter MODE-FILTER is currently applicable."
  (or (null mode-filter)
      (cl-find major-mode mode-filter)
      (cl-loop for mode in mode-filter
               thereis (and (boundp mode)
                            (symbol-value mode)))))

(defun autobuild-current-build-actions ()
  "Return a list of the currently applicable build actions.

   A rule RULE is applicable if the current major mode is contained in the
   rule's list of major modes, and if the rule generates a non-nil action."
  (cl-loop for rule in (reverse autobuild-rules-list)
           as action-nice = (if-let* ((autobuild-nice autobuild-nice-default)
                                      (action (autobuild-rule-action rule)))
                                (cons action autobuild-nice))
           when action-nice
           collect (cl-destructuring-bind (action . nice) action-nice
                     (list rule action nice))
           into name-action-nice-list
           finally (return (autobuild--sort-by #'cl-third name-action-nice-list))))

;; TODO rename to autobuild-last-rule
(defvar-local autobuild-last-rule-name nil)

(defun autobuild--sort-by (key list)
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
                         (let* ((last-rule autobuild-last-rule-name)
                                action)
                           (if (null last-rule)
                               (progn (warn "rule no longer exists: %s" autobuild-last-rule-name)
                                      nil)
                             (when (setq action
                                         (autobuild-rule-action last-rule))
                               (list (list autobuild-last-rule-name
                                           last-rule
                                           action))))))
                    (autobuild-current-build-actions)))
         (choice (cond ((null cands) (error "No build rules matched"))
                       ((not prompt) (car cands))
                       (t (selcand-select cands "select build rule: "
                                          ;; TODO sort vertically
                                          (lambda (rule-action-nice)
                                            (cl-destructuring-bind (rule _action nice)
                                                rule-action-nice
                                              (format "%s (%s)" rule nice))))))))
    (cl-assert choice)
    (setq autobuild-last-rule-name (car choice))
    (cl-destructuring-bind (_rule action _nice) choice
      (autobuild-run-action (cl-second choice)))))

(defvar autobuild-last-executed-action nil)

(defun autobuild-run-action (action)
  "Execute a rule-generated ACTION as specified in ‘autobuild-define-rule'."
  (cl-assert action)
  (setq autobuild-last-executed-action (cons action (current-buffer)))
  (cond
   ((stringp action) (autobuild-run-string-command action))
   ((commandp action) (call-interactively action))
   ((functionp action) (funcall action))
   (t (error "Action must be string or function, not %s" action))))

(defun autobuild-rebuild-last-action ()
  "Rerun the last autobuild action."
  (interactive)
  (if (not autobuild-last-executed-action)
      (error "No last known action")
    (cl-destructuring-bind (action . buffer)
        autobuild-last-executed-action
      (if (not (buffer-live-p buffer))
          (error "Buffer not live: %s" buffer)
        (with-current-buffer buffer
          (autobuild-run-action action))))))

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

(defcustom autobuild-notify-threshold-secs 10
  "Min seconds elapsed since compilation start before a notification is issued.

  If nil, disable notifications.
  If t, always issue notifications."
  :type 'number
  :group 'autobuild)

(defcustom autobuild-notification-function
  #'autobuild-notification-default-function
  "Function used to issue compilation notifications.

   It is called with the same arguments as those in ‘compilation-finish-functions'"
  :type 'function
  :group 'autobuild)

(defun autobuild-notification-default-function (_ compilation-state)
  "Default, simple autobuild notification function.

   This may be redefined with a more fancy notification mechanism,
   e.g. notify-send desktop notifications, audible beep, etc.

   COMPILATION-STATE is as described in ‘compilation-finish-functions'"
  (message "compilation %s: %s"
           (replace-regexp-in-string "\n" " " compilation-state)
           ;; TODO this is a global. this may fail if
           ;; compilation-command is not updated, e.g. build-cleaner
           compile-command))

(defun autobuild-notify (compilation-buffer compilation-state)
  "Hook function called to possibly issue compilation state notifications.

   COMPILATION-BUFFER, COMPILATION-STATE are as described in ‘compilation-finish-functions'"
  (condition-case ex
      (when compilation-state
        (with-current-buffer compilation-buffer
          (when (and
                 ;; this fails when emacs is not raised and therefore not visible...
                 ;; (not (frame-visible-p (selected-frame)))
                 autobuild-notify-threshold-secs
                 (or (eq autobuild-notify-threshold-secs t)
                     (>= (- (time-to-seconds)
                            autobuild-compilation-start-time)
                         autobuild-notify-threshold-secs)))
            (funcall autobuild-notification-function
                     compilation-buffer compilation-state))))
    (error
     ;; avoid interrupting compilation-finish-functions due to
     ;; errors in potentially user-provided ‘autobuild-notification-function'
     (message (format "Error in autobuild-notify: %s" ex)))))


;; TODO use pipeline hook, not compilation hook
(add-hook 'compilation-finish-functions 'autobuild-notify)

(defun autobuild-delete-rule (rule)
  "Delete the RULE from the autobuild rules registry."
  (interactive
   (list (selcand-select (mapcar #'car autobuild-rules-list)
                         "select rule to delete: ")))
  (cl-assert (assoc rule autobuild-rules-list))
  (setq autobuild-rules-list
        (assq-delete-all rule autobuild-rules-list)))

;; TODO support autobuild-next-buffer and defining one-off pipelines interactively

(autobuild-define-rule autobuild-emacs-lisp-eval-buffer (emacs-lisp-mode)
  "Evaluate the current emacs-lisp buffer"
  #'eval-buffer)

(provide 'autobuild)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autobuild.el ends here
