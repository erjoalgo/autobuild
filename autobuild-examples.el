;;; autobuild-examples.el --- Sample rules for autobuild.el
;;
;; Filename: autobuild-examples.el
;; Description:
;; Author: Ernesto Alfonso
;; Maintainer:
;; Created: Thu Jan 24 00:46:25 2019 (-0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 0
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Sample rules for autobuild.el
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(require 'autobuild)
(require 'cl-lib)
(require 'f)
(require 's)


(autobuild-define-rule autobuild-file-local-compile-command nil
  "Set and run the file-local compile command"
  (when (buffer-file-name)
    (autobuild-nice
       (if (alist-get 'compile-command file-local-variables-alist)
           8 12))
    (lambda ()
      (let* ((command
              (if (and (bound-and-true-p compile-command)
                       (not current-prefix-arg))
                  compile-command
                (read-shell-command "enter compile command: "
                                    (when (bound-and-true-p compile-command)
                                      compile-command)))))
        (unless (equal command compile-command)
          (add-file-local-variable 'compile-command command)
          (setq compile-command command))
        (compile compile-command)))))

(autobuild-define-rule autobuild-editor-done (fundamental-mode)
  (lambda ()
    (save-buffer)
    (server-edit)))

(autobuild-define-rule autobuild-run-executable nil
  (let ((filename (buffer-file-name)))
    (when (and filename
               (file-executable-p filename))
      (autobuild-nice 7)
      (format "./%s" (f-filename filename)))))

(autobuild-define-rule autobuild-dired-build-file-at-point (dired-mode)
  "Build the file at point"
  (when (dired-file-name-at-point)
    (lambda ()
      (save-excursion
        (with-current-buffer
            (find-file-noselect (dired-file-name-at-point))
          (call-interactively #'autobuild-build))))))

(autobuild-define-rule autobuild-shell-script-run (sh-mode)
  (let ((fn (f-filename (buffer-file-name))))
    (format "bash %s" fn)))

(autobuild-define-rule autobuild-shell-script-syntax-check (sh-mode)
  "Syntax-check a bash script without running it"
  (autobuild-nice 15)
  (let ((fn (f-filename (buffer-file-name))))
    (format "bash -n %s" fn)))

(autobuild-define-rule autobuild-java-mode (java-mode nxml-mode)
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
                (when (bound-and-true-p mvn-offline-p) "-o ")
                ;;always clean
                "clean "
                ;; verify or install
                (cond
                 ((s-ends-with-p "IT" f-no-ext) "verify ")
                 (t "install "))
                ;;maybe add -s *_settings.xml
                (let* ((mvn-settings (cl-remove-if-not
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

                (when (bound-and-true-p mvn-extra-args) (concat mvn-extra-args " ")))))))

(autobuild-define-rule autobuild-cl-slime-eval (lisp-mode)
  "Evaluate the current lisp buffer"
  #'slime-compile-and-load-file)

(autobuild-define-rule autobuild-cl-asdf (lisp-mode)
  (let ((filename (f-filename (buffer-file-name))))
    (when (member (f-ext filename) '("asd" "asdf"))
      (format "sbcl --load %s --eval \"(ql:quickload '%s)\""
              filename
              (f-base filename)))))

(autobuild-define-rule autobuild-el-run-tests (emacs-lisp-mode)
  "Run emacs lisp tests."
  (when (string-match-p "-tests?.el" (buffer-file-name))
    (autobuild-nice 8)
    (lambda () (eval-buffer) (ert (regexp-quote (f-base buffer-file-name))))))

(autobuild-define-rule autobuild-el-run-tests-interactively (emacs-lisp-mode)
  "Run emacs lisp tests."
  (when (string-match-p "-tests?.el" (buffer-file-name))
    (autobuild-nice 9)
    #'ert-run-tests-interactively))

(autobuild-define-rule autobuild-makefile-make nil
  "Run make"
  (when (file-exists-p "Makefile") "make"))

(autobuild-define-rule autobuild-makefile-make-clean nil
  "Run make clean"
  (when (file-exists-p "Makefile") "make clean"))

(autobuild-define-rule autobuild-configure-make-install nil
  (lexical-let ((autogen
                 (when (file-exists-p "autogen.sh")
                   (find-file-noselect "autogen.sh")))
                (configure
                 (when (file-exists-p "configure")
                   (find-file-noselect "configure"))))
    (when (or autogen configure)
      (autobuild-pipeline
       (autogen "./autogen.sh")
       (configure "./configure")
       ((find-file-noselect "Makefile") "make")
       ((find-file-noselect "Makefile") "sudo make install")))))

(autobuild-define-rule autobuild-mpm nil
  (when (and (buffer-file-name)
             (equal "pkgdef" (f-ext (buffer-file-name))))
    (format "mpm build --pkgdef_file=%s --alsologtostderr"
            (buffer-file-name (current-buffer)))))

(autobuild-define-rule autobuild-c (c-mode)
  (let ((fn (f-filename (buffer-file-name)))
        (pipe-in (if (file-exists-p "test.in") " < test.in" ""))
        (speed (if (bound-and-true-p c-ofast-compilation)
                   "-Ofast" "-g")))
    (format "gcc %s -Wall -W -std=c99 -Wextra -lm %s && ./a.out %s %s"
            speed fn
            (if (bound-and-true-p executable-args)
                executable-args "")
            pipe-in)))

(autobuild-define-rule autobuild-c++ (c++-mode)
  (let ((fn (f-filename (buffer-file-name)))
        (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
    (format "g++ %s -std=c++11 && ./a.out %s"
            fn pipe-in)))

(autobuild-define-rule autobuild-go (go-mode)
  "go test")

(autobuild-define-rule autobuild-latex (tex-mode latex-mode)
  'latex-compile)

(autobuild-define-rule autobuild-python-run (python-mode)
  (format "python %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-python3-run (python-mode)
  (format "python3 %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-git-finish nil
  (when (or (eq major-mode 'git-rebase-mode)
            (and (eq major-mode 'text-mode)
                 (equal (f-filename (buffer-file-name)) "COMMIT_EDITMSG")))
    (autobuild-nice 8)
    (lambda ()
      (progn
        (save-buffer)
        (with-editor-finish nil)))))

(autobuild-define-rule autobuild-diff (diff-mode)
  (lambda ()
    (progn (save-buffer)
           (server-edit))))

(autobuild-define-rule autobuild-clojure (clojure-mode)
  #'cider-load-buffer)

(autobuild-define-rule autobuild-send-message (message-mode)
  #'message-send-and-exit)

(autobuild-define-rule autobuild-org-export-html (org-mode)
  (lambda ()
    (let* ((fn (org-html-export-to-html))
	   (url (format "file://%s" (f-full fn)))
	   (org-exporting-mine t))
      (browse-url url))))

(autobuild-define-rule autobuild-org-export-pdf (org-mode)
  (autobuild-nice 11)
  #'org-latex-export-to-pdf)

(autobuild-define-rule autobuild-org-export-odt (org-mode)
  (autobuild-nice 11)
  #'org-odt-export-to-odt)

(autobuild-define-rule autobuild-octave-eval (octave-mode)
  (if (region-active-p)
      (apply-partially #'call-interactively #'octave-send-region)
    #'octave-send-buffer))

(autobuild-define-rule autobuild-html-browse (html-mode mhtml-mode)
  "Open the current html file in the browser"
  (let ((url (format "file://%s" (buffer-file-name))))
    (apply-partially #'browse-url url)))

(autobuild-define-rule autobuild-node-run (js-mode)
  (let ((filename (f-filename (buffer-file-name))))
    (format "node %s" filename)))

(autobuild-define-rule autobuild-cfboot (js-mode)
  (let ((filename (f-filename (buffer-file-name))))
    (when (s-ends-with-p "-boot.json" filename)
      (format "cf-boot %s -i free-vars.json" filename))))

(autobuild-define-rule autobuild-texinfo-build (texinfo-mode)
  (concat "texi2any " (buffer-file-name)
          " --html"
          " --no-number-sections"))

(autobuild-define-rule autobuild-nginx-restart (nginx-mode)
  "sudo service nginx restart")

(autobuild-define-rule autobuild-nginx-test-config (nginx-mode)
  "sudo nginx -t")

(autobuild-define-rule autobuild-ispell (text-mode org-mode)
  "Do a spell check"
  #'ispell)

(autobuild-define-rule autobuild-json-syntax-check (js-mode)
  "Check the syntax of a json file"
  ;; ensure we are in a JSON file
  (when (and (buffer-file-name)
             (equal "json" (f-ext (buffer-file-name))))
    (format "python -m json.tool < %s" (f-filename (buffer-file-name)))))

(autobuild-define-rule autobuild-python-setupy-install (python-mode)
  "Run setup.py install"
  (when (and (buffer-file-name)
             (equal "setup.py"
                    (f-filename (buffer-file-name))))
    (format "python %s install --user" (f-filename (buffer-file-name)))))

(autobuild-define-rule autobuild-xmodmap (conf-unix-mode)
  "run xmodmap on a file"
  (autobuild-nice 9)
  (when (and (buffer-file-name)
             (equal "xmodmap"
                    (f-ext (buffer-file-name))))
    (format "xmodmap -verbose %s" (f-filename (buffer-file-name)))))

(autobuild-define-rule autobuild-message-send (message-mode)
  "Send an email in gnus message-mode"
  #'message-send-and-exit)

(autobuild-define-rule autobuild-dot-to-ps (graphviz-dot-mode)
  "Convert a .dot file to ps."
  (when-let ((buffer-file-name)
             (file (f-filename buffer-file-name)))
    (format "dot -Tps %s -o %s.ps"
            file file)))

(autobuild-define-rule autobuild-python-pylint (python-mode)
  #'python-check)

(provide 'autobuild-examples)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autobuild-examples.el ends here
