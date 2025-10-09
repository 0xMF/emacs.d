;;; package -- init.el

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.
;;

;;; Code:

;; bring in my preferred packages specified in custom.el/package-selected-packages
(unless package-archive-contents
  (package-refresh-contents))

(setq 0xMF/required-packages '(djvu exec-path-from-shell evil fill-column-indicator general go-mode
                                    hide-mode-line hindent htmlize keychain-environment markdown-mode
                                    nov org-bullets org-noter-pdftools org-pdftools slime ssh-agency
                                    undo-fu use-package vline yafolding))

(defun 0xMF/update-package-selected-packages()
  (interactive)
  (dolist (package 0xMF/required-packages)
    (add-to-list 'sanityinc/required-packages package)
    (append package-selected-packages package)
    (unless  (package-installed-p package)
      (package-install package)
      (message "installed package %s" package))))
(0xMF/update-package-selected-packages)

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-check-startup-files nil)
(setq exec-path (append exec-path '("~/bin")))
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(exec-path-from-shell-initialize)

(defun load-if-file-exists (FILE)
  "Check if FILE exists before loading it."
  (if (file-readable-p FILE)
      (load-file (expand-file-name FILE))))

(load "init-0xMF-evil")
(load "init-0xMF-misc")
(load "init-0xMF-org")
(load "init-0xMF-zsec" t)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)

;;;  -*- mode: Lisp;-*
;;; init-local.el ends here
