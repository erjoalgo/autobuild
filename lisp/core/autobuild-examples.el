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
(require 'f)
(require 's)

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
                       (lambda ()
                         (progn
                         (save-buffer)
                         (with-editor-finish nil))))

(autobuild-define-rule autobuild-diff
                       (diff-mode)
                       (lambda ()
                         (progn (save-buffer)
                              (server-edit))))

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
;;; autobuild-examples.el ends here
