;;; package --- 0xMF preferences for .emacs

;;; Commentary:

;;  - relies on purcell's .emacs.d to do all the heavy lifting
;;  - all else is kept to bare minimum.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)
;;(setq package-check-signature nil)

;; grab all files in lisp and any sub-dirs inside it
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; silence ad-handle-redefinition warnings
(setq ad-redefinition-action 'accept)
;; evil, evil-collection, and evil-magit should play nice with each other
(setq evil-want-keybinding 'nil)

;; force TLS1.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(load "~/.emacs.d/init")

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'emacs)
;;; emacs.el ends here
