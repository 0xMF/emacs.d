;;; init-0xMF-org.el --  -*- lexical-binding: t -*-
;;; -----------------
;;
;;; package: customize Purcell's Emacs setup with my org+evil-related preferences
;;
;;; Commentary:
;;
;;  all my org-mode preferences and customizations are here.

;;----------------------------------------------------------------------------
;; Org mode settings
;;----------------------------------------------------------------------------
(setq org-export-with-section-numbers nil)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-indent-mode-turns-off-org-adapt-indentation nil)
(setq org-list-allow-alphabetical t)
(setq org-pretty-entities t)
(setq org-startup-align-all-table t)
(setq org-startup-folded 'content)
(setq org-startup-indented nil)
(setq org-table-auto-blank-field nil)
(setq org-time-stamp-custom-formats '("<%b-%d %a>" "<%m/%d/%y %a>" "<%m/%d/%y %a %H:%M>"))

;; Use bullets (default if uncommented)
(require 'use-package)
(require 'org-bullets)
(require 'org-tempo)
;;(require 'org-gcal)
 (use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(pdf-tools-install)

(use-package org-noter-pdftools
   :after org-noter
   :config
   (with-eval-after-load 'pdf-annot
     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
 (add-to-list 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)
(eval-after-load "org" '(require 'org-pdftools))

(defun 0xMF/org/shrink ()
  "Shrink table according to cookie at point."
  (interactive)
  (org-table-shrink))

;; do not ask before prompting
(setq org-confirm-babel-evaluate nil)

;; output htmlize as css
;; refer: https://github.com/gongzhitaao/orgcss
(setq org-html-htmlize-output-type 'css)

(setq-default major-mode 'org-mode)

;; important for markdown, GFM export, and viewing pdfs
(eval-after-load "org" '(require 'ox-md nil t))
(eval-after-load "org" '(require 'ox-gfm nil t))
(eval-after-load "org" '(require 'htmlize))

(defun 0xMF/settings/orgmode ()
  "My Org settings.
Turn on spell check automatically; maketext wrap at 81; and make
'org-mode' default for scratch (new) buffers."
  (interactive)
  (setq initial-major-mode 'org-mode)
  (org-bullets-mode 1)
  (turn-off-flyspell)
  (set-fill-column 81))
(add-hook 'org-mode-hook '0xMF/settings/orgmode)

(defun 0xMF/orgmode-remove-tag (tag)
  "Remove TAG from line."
  (interactive "sTag:")
  (org-toggle-tag tag 'off))

(defun 0xMF/settings/orgmode-emphasis-markers-reset ()
  "Reset emphasis markers in current buffer."
  (interactive)
  (setq org-hide-emphasis-markers t))

(defun 0xMF/settings/orgmode-emphasis-markers-toggle ()
  "Toggle emphasis markers in current buffer."
  (interactive)
  (org-toggle-link-display)
  (setq org-hide-emphasis-markers (if (eq org-hide-emphasis-markers nil) t nil))
  (font-lock-fontify-buffer))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-0xMF-org)
;;; init-0xMF-org.el ends here.
