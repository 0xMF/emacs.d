;;; package -- init.el

;;; Commentary:

;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.
;;

;;; Code:

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-check-startup-files nil)
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq exec-path (append exec-path '("~/bin")))
(exec-path-from-shell-initialize)

;; bring in my preferred packages specified in custom.el/package-selected-packages
(unless package-archive-contents
  (package-refresh-contents))

(setq 0xMF/required-packages '(djvu evil fill-column-indicator general go-mode hide-mode-line
                                    keychain-environment org-bullets org-pdftools
                                    org-noter-pdftools ssh-agency undo-fu use-package
                                    vline yafolding))

(defun 0xMF/update-package-selected-packages()
  (interactive)
  (dolist (package 0xMF/required-packages)
    (append package-selected-packages package)
    (unless  (package-installed-p package)
      (package-install package)
      (message "installed package %s" package))))
(0xMF/update-package-selected-packages)

(defun load-if-file-exists (FILE)
  "Check if FILE exists before loading it."
  (if (file-readable-p FILE)
      (load-file FILE)))

(load "init-0xMF-evil")
(load "init-0xMF-org")
(load "init-0xMF-misc")
(load-if-file-exists (expand-file-name "~/.emacs.d/lisp/secrets.el"))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-local)

;;;  -*- mode: Lisp;-*
;;; init-local.el ends here
