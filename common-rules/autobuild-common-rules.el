;;; autobuild-common-rules.el --- Sample rules for autobuild.el  -*- lexical-binding: t; -*-
;;
;; Filename: autobuild-common-rules.el
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
;; Common autobuild.el rules.
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
         5 12))
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

(autobuild-defvar-file-local configure-flags "./configure AC script flags")

(autobuild-define-rule autobuild-run-executable nil
  (let* ((filename (buffer-file-name))
         (base (f-filename filename))
         cmdline)
    (when (and filename
               (file-executable-p filename))
      (autobuild-nice 7)
      (lambda ()
        (cond
         ((equal base "configure")
          (setq cmdline
                (format "./%s %s" base (bound-and-true-p configure-flags))))
         (t (setq cmdline (format "./%s" (f-filename filename)))))
        (compile cmdline t)))))

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
    (let ((f-no-ext (f-no-ext (f-filename (buffer-file-name))))
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
  (autobuild-nice 12)
  (when (file-exists-p "Makefile") "make"))

(autobuild-define-rule autobuild-makefile-make-clean nil
  "Run make clean"
  (autobuild-nice 13)
  (when (file-exists-p "Makefile") "make clean"))

(defun autobuild-directory-makeable ()
  (or
   (file-exists-p "autogen.sh")
   (file-expand-wildcards "configure*")
   (file-expand-wildcards "Makefile*")))

(autobuild-define-rule autobuild-configure-make-install nil
  (when (autobuild-directory-makeable)
    (lambda ()
      (let* (commands
             (autogen (car (file-expand-wildcards "./autogen.sh")))
             (configure (car (file-expand-wildcards "./configure")))
             (configure-acs (file-expand-wildcards "./configure.ac*"))
             (makefile (car (file-expand-wildcards "Makefile"))))
        (when autogen (push autogen commands))
        (cond
         (configure (push configure commands))
         (configure-acs
          '(when (cdr configure-acs)
             (push (format "cp %s configure.ac" (selcand-select configure-acs)) commands))
          (push "autoreconf -i" commands)
          (push (format "./configure %s" (or (bound-and-true-p configure-flags) ""))  commands))
         ((not makefile)
          (error "No configure* nor makefile found!")))
        (when (bound-and-true-p autobuild-make-clean)
          (push "make clean" commands))
        (push "make" commands)
        (compile (string-join (nreverse commands) " && "))))))

(autobuild-define-rule autobuild-make-clean-install nil
  "Run sudo make install."
  (autobuild-nice 13)
  (when (autobuild-directory-makeable)
    (let ((autobuild-make-clean t))
      (autobuild-configure-make-install))))

(autobuild-define-rule autobuild-sudo-make-install nil
  "Run sudo make install."
  (autobuild-nice 13)
  (when (autobuild-directory-makeable)
    "sudo make install"))

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

(autobuild-defvar-file-local gcc-flags "")

(autobuild-define-rule autobuild-c++ (c++-mode)
  (let ((fn (f-filename (buffer-file-name)))
        (pipe-in (if (file-exists-p "test.in") " < test.in" "")))
    (format "g++ %s -std=c++17 %s && ./a.out %s"
            fn (or (bound-and-true-p gcc-flags) "") pipe-in)))

(autobuild-define-rule autobuild-valgrind (c++-mode)
  (when (file-exists-p "a.out")
    (autobuild-nice 13)
    (lambda ()
      (compile (autobuild-c++-debug))
      (compile (format "valgrind ./a.out")))))

(autobuild-define-rule autobuild-go-test (go-mode)
  "go test")

(autobuild-define-rule autobuild-go-run (go-mode)
  (lambda ()
    ;; maybe kill the last compilation, if it was a "go run"
    (if-let* ((compilation "*compilation*")
              (buffer (get-buffer compilation)))
        (with-current-buffer buffer
          (when (bound-and-true-p autobuild-go-run-compilation)
            (kill-buffer))))
    (let* ((cmd (concat "go run " (f-filename (buffer-file-name))))
           (buffer (compile cmd)))
      (with-current-buffer buffer
        (setq autobuild-go-run-compilation t)))))

(autobuild-define-rule autobuild-go-install (go-mode)
  "go install")

(autobuild-define-rule autobuild-latex (tex-mode latex-mode)
  'latex-compile)

(autobuild-define-rule autobuild-python-run (python-mode)
  (format "python %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-python3-run (python-mode)
  (autobuild-nice 8)
  (format "python3 %s" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-git-finish (editorconfig-mode text-mode git-rebase-mode)
  (when (or (eq major-mode 'git-rebase-mode)
            (and (eq major-mode 'text-mode)
                 (member (f-filename (buffer-file-name))
                         '("COMMIT_EDITMSG" "TAG_EDITMSG"))))
    (autobuild-nice 8)
    (lambda ()
      (progn
        (save-buffer)
        (with-editor-finish nil)))))

(autobuild-define-rule autobuild-rebase-finish (git-rebase-mode)
  (autobuild-nice 8)
  (lambda ()
    (progn
      (save-buffer)
      (with-editor-finish nil))))

(autobuild-define-rule autobuild-git-commit-editmsg (text-mode)
  (when (equal (buffer-name) "COMMIT_EDITMSG")
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

(autobuild-defvar-file-local node-trace-deprecation ""
  (y-or-n-p "pass --trace-deprecation to node?"))

(autobuild-define-rule autobuild-node-run (js-mode)
  (let ((filename (f-filename (buffer-file-name)))
        flags)
    (when (bound-and-true-p node-trace-deprecation)
      (push "--trace-deprecation" flags))
    (format "node %s %s" (string-join flags " ") filename)))

(autobuild-define-rule autobuild-node-inspect (js-mode)
  (let ((filename (f-filename (buffer-file-name))))
    (lambda ()
      (compile (format "node inspect %s" filename) t))))

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

(autobuild-define-rule autobuild-python-setupy-install (python-mode dired-mode)
  "Run setup.py install"
  (when (or
         (and (buffer-file-name)
              (equal "setup.py"
                     (f-filename (buffer-file-name))))
         (file-exists-p "setup.py"))
    (format "python setup.py install --user")))

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

(autobuild-define-rule autobuild-dot-to-ps ()
  "Convert a .dot file to ps."
  (autobuild-nice 8)
  (when-let ((buffer-file-name)
             (file (f-filename buffer-file-name))
             (_applicable
              (or (bound-and-true-p graphviz-dot-mode)
                  (and (buffer-file-name)
                       (equal "dot" (f-ext file))))))
    (format "dot -Tps %s -o %s.ps && evince %s.ps"
            file file (f-filename file))))

(autobuild-define-rule autobuild-python-pylint (python-mode)
  #'python-check)

(autobuild-define-rule autobuild-python-modernize (python-mode)
  (format "python-modernize %s -w" (f-filename (buffer-file-name))))

(autobuild-define-rule autobuild-systemd-lint ()
  (save-match-data
    (when (and
           (buffer-file-name)
           (string-match "/etc/systemd/system/[^/]+.service"
                         (buffer-file-name)))
      (autobuild-nice 8)
      (format "sudo systemd-analyze verify %s"
              (f-filename (buffer-file-name))))))

(autobuild-define-rule autobuild-ps2pdf (ps-mode)
  "Evaluate the current emacs-lisp buffer"
  (format "ps2pdf %s" (shell-quote-argument
                       (f-filename (buffer-file-name)))))

(autobuild-define-rule autobuild-docker-compose (conf-colon-mode)
  (when (equal "docker-compose.yml" (f-filename (buffer-file-name)))
    (format "sudo docker-compose up")))

(defun service-restart-and-tail (service-name)
  (let ((buffer (get-buffer-create (format "*service-logs-%s*" service-name))))
    (with-current-buffer buffer
      (unless (get-buffer-process (current-buffer))
        (start-process (buffer-name) (current-buffer)
                       "sudo" "journalctl" "-fu" service-name))
      (erase-buffer)
      (start-process (buffer-name) nil
                     "sudo" "service" service-name "restart")
      (display-buffer (current-buffer))
      (with-current-buffer buffer
        (end-of-buffer-other-window nil)))))


(autobuild-define-rule autobuild-service-restart-and-tail (conf-space-mode)
  "Restart a service and tail its logs."
  (let ((filename (car (last (s-split ":" (buffer-file-name (current-buffer)))))))
    (cond
     ((equal filename "/etc/dhcp/dhcpd.conf")
      (autobuild-nice 6)
      (apply-partially #'service-restart-and-tail "isc-dhcp-server")))))


(provide 'autobuild-common-rules)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autobuild-common-rules.el ends here
