;;; init-0xMF-evil.el --  -*- lexical-binding: t -*-
;;; -----------------
;;
;;; package: override Purcell's Emacs with my evil preferences
;;
;;; Commentary:
;;
;;  purcell/emacs.d provides basic functionality that I am interested in using
;;  however not all settings are required by me and so this simple configuration
;;  setup to include my local settings after purcell/emacs.d has been run.
;;
;;; Code:

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

(require 'exec-path-from-shell)
(setq exec-path-from-shell-arguments nil)
(setq exec-path-from-shell-check-startup-files nil)
(setenv "PATH" (concat (getenv "PATH") ":~/bin"))
(setq exec-path (append exec-path '("~/bin")))
(exec-path-from-shell-initialize)

;; disable cl-lib deprecated warnings
(setq byte-compile-warnings '(cl-functions))

(setq vc-follow-symlinks t)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; prefer newer source instead of older bytecode
(setq load-prefer-newer t)

(keychain-refresh-environment)

;; use Noto Color Emoji for emoji support
(when (member "Noto Color Emoji" (font-family-list))
     (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend))

;;----------------------------------------------------------------------------
;; Evil mode settings
;;----------------------------------------------------------------------------
(require 'evil)
(require 'general)
(require 'undo-fu)

(defun 0xMF/settings/cursor-emacs-evil-toggle()
  "Cursor color indicates mode: white = Emacs, green = evil (Vi/Vim)."
  (interactive)
  (if (string= (symbol-value 'evil-state) "normal")
      (set-cursor-color "green")
    (set-cursor-color "white")))

(defun 0xMF/settings/cursor-column-toggle()
  "Show and follow cursor column."
  (interactive)
  (vline-mode 'toggle)
  (if (bound-and-true-p vline-face)
      (progn
        (if (string= (face-background 'default) "#f5f5dc")
            (set-face-background vline-face "light steel blue")
          (set-face-background vline-face "firebrick")))))

(defun 0xMF/settings/cursor-toggle ()
  "Toggle showing cursor."
  (interactive)
  (if (boundp pdf-view-display-size)
      (internal-show-cursor nil nil))
  (if (internal-show-cursor-p)
      (internal-show-cursor nil nil)
    (internal-show-cursor nil t)))
(evil-mode 1)

(setq evil-default-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-normal-state-cursor '("green" box))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-replace-state-cursor '("red" box))
(setq evil-visual-state-cursor '("orange" box))

;; better clipboard copy-paste with evil
(fset 'evil-visual-update-x-selection 'ignore)

(add-hook 'evil-mode-hook '0xMF/settings/cursor-emacs-evil-toggle)
(add-hook 'calendar-mode-hook '0xMF/settings/calendar-mode)
(add-hook 'package-menu-mode-hook '0xMF/settings/package-menu-mode)

;;----------------------------------------------------------------------------
;; General keymap settings
;;----------------------------------------------------------------------------

;; bind a key globally in normal state
(setq general-default-keymaps 'evil-normal-state-map)
;; named prefix key allows ; to be used a mapper for my keybindings
;; bind a key globally in normal state
(setq general-default-keymaps 'evil-normal-state-map)
(setq 0xMF-leader1 ";")
;;"Bind keys in multiple states of Org-mode."
(general-define-key :keymaps 'org-mode-map
                    :states '(insert emacs)
                    "<tab>" 'org-cycle)


(defun 0xMF/settings/general ()
  "General keyboard settings."
  (interactive)

  ;; bind j and k in normal state globally
  (general-define-key "j" 'evil-next-visual-line
                      "k" 'evil-previous-visual-line
                      "SPC" 'evil-scroll-page-down
                      "DEL" 'evil-scroll-page-up)

  ;; bind wm and wc
  (general-define-key :prefix "w"
                      "a" 'org-toggle-link-display ; 'beginning-of-line
                      "c" 'whitespace-cleanup
                      "f" '0xMF/toggle-font-large-normal
                      "|" 'fci-mode
                      "h" 'previous-buffer
                      "j" 'next-buffer
                      "k" 'kill-this-buffer
                      "l" 'previous-buffer
                      "m" 'writeroom-mode
                      "n" 'next-buffer
                      "N" 'other-window
                      "t" 'whitespace-mode
                      "o" #'(lambda () (interactive) (other-window 1))
                      "O" 'delete-other-windows
                      "p" 'previous-buffer
                      "r" 'evil-window-rotate-upwards
                      "R" 'evil-window-rotate-downwards
                      "u" 'winner-undo
                      "U" 'winner-redo
                      "w" 'delete-other-windows
                      "W" 'delete-window
                      "0" 'delete-window
                      "1" 'delete-window)

  (general-define-key :prefix "b"
                      "a" 'describe-bindings
                      "b" 'evil-scroll-page-up
                      "c" 'yank
                      "d" 'org-time-stamp-inactive
                      "D" 'org-time-stamp
                      "f" 'markdown-follow-thing-at-point
                      "h" 'previous-buffer
                      "j" 'next-buffer
                      "l" 'list-buffers
                      "n" 'next-buffer
                      "o" 'org-open-at-point
                      "p" 'previous-buffer
                      "t" #'(lambda () (interactive) (kill-buffer)(delete-window))
                      "T" 'sanityinc/toggle-delete-other-windows
                      "x" 'evil-delete)

  (general-define-key :prefix "z"
                      "b" 'paredit-forward-barf-sexp
                      "B" 'paredit-backward-barf-sexp
                      "c" 'comment-or-uncomment-region
                      "e" 'eval-last-sexp
                      "E" 'eval-region
                      "d" #'yafolding-toggle-all
                      "f" 'paredit-forward-slurp-sexp
                      "F" 'paredit-backward-slurp-sexp
                      "k" #'0xMF/shrink
                      "g" 'save-this-word
                      "o" 'org-open-at-point
                      "O" 'org-open-at-point
                      "n" 'menu-bar--display-line-numbers-mode-relative
                      "s" 'paredit-forward-slurp-sexp
                      "S" 'paredit-backward-slurp-sexp
                      "t" 'save-this-word
                      "x" '0xMF/orgmode-remove-tag
                      "y" #'yafolding-toggle-element)

  (general-define-key :prefix 0xMF-leader1
                      "a" '0xMF/settings/orgmode-emphasis-markers-toggle
                      "A" 'org-agenda
                      "B" 'switch-to-buffer
                      "d" 'insdate-insert-current-date
                      "D" 'org-agenda-list
                      "e" 'eval-last-sexp
                      "E" 'eval-region ;; org-babel-execute-src-block org-babel-open-src-block-result
                      "f" 'set-fill-column
                      "F" 'file-reload ; 'fill-paragraph
                      "g" '0xMF/settings/cursor-column-toggle ;; magit-status
                      "i" '0xMF/settings/Info-mode
                      "k" 'kill-this-buffer
                      "l" 'whitespace-mode
                      "L" 'org-open-at-point
                      "m" '0xMF/settings/hide-mode-line-toggle
                      "N" 'menu-bar--display-line-numbers-mode-absolute
                      "o" 'find-file
                      "O" 'org-open-at-point
                      "q" 'delete-other-windows ;;'toggle-truncate-lines ;;'visual-line-mode ;;fill-paragraph
                      "R" 'file-reload ;;'undo-tree-redo
                      "s" '0xMF/startup
                      "u" 'undo-tree-undo
                      "w" 'toggle-truncate-lines
                      "W" #'(lambda () (interactive) (org-agenda-list 7))
                      "x" 'evil-delete
                      "y" 'timeclock-out
                      "Y" 'timeclock-in
                      "z" '0xMF/zero
                      "+" #'(lambda () (interactive) (text-scale-increase 2))
                      "=" #'(lambda () (interactive) (text-scale-increase 3))
                      "-" 'text-scale-decrease
                      "0" #'(lambda () (interactive) (text-scale-adjust 0))
                      "$" 'toggle-truncate-lines
                      "<SPC>" 'recenter-top-bottom
                      ";" 'org-capture
                      "\\" 'org-match-sparse-tree))

;; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(defun 0xMF/settings/vi ()
  "My Vi settings."
  ;; jump j/k always even in visual mode
  (interactive)
  (turn-on-evil-mode)
  (0xMF/settings/general)
  (dolist (map  (list evil-normal-state-map))
    (define-key map (kbd "j") 'evil-next-visual-line)
    (define-key map (kbd "k") 'evil-previous-visual-line)
    (define-key map (kbd "p") 'evil-paste-after)
    (define-key map (kbd "q") #'(lambda () (interactive) (unless (one-window-p) (delete-other-windows)) (keyboard-quit)))
    (define-key map (kbd "u") 'undo)
    (define-key map [escape]  #'(lambda () (interactive) (unless (one-window-p) (delete-other-windows)) (keyboard-quit)))
    (define-key map [prior] 'evil-scroll-page-up)
    (define-key map [next] 'evil-scroll-page-down)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-j") #'(lambda () (interactive) (evil-scroll-down nil)))
    (define-key map (kbd "C-d") 'save-buffer)
    (define-key map (kbd "C-n") 'next-buffer)
    (define-key map (kbd "C-p") 'previous-buffer)
    (define-key map (kbd "C-r") 'undo-fu-only-redo)
    (define-key map (kbd "M-n") 'evil-scroll-page-down)
    (define-key map (kbd "M-p") 'evil-scroll-page-up)))

(define-key minibuffer-local-map [tab] 'vertico-insert)
(define-key minibuffer-local-map (kbd "M-<return>") 'vertico-exit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(when (boundp 'company-mode-map)
  (define-key company-active-map [tab] 'company-complete-common))
                                        ;(dolist (map  (list company-active-map))
                                        ;  (define-key map (kbd "<tab>") 'company-complete-common))

(dolist (map  (list minibuffer-local-isearch-map))
  (define-key map (kbd "n") 'isearch-printing-char))

(global-set-key [escape] 'evil-exit-emacs-state)

(define-key evil-normal-state-map (kbd "C-k") #'(lambda () (interactive) (evil-scroll-up nil)))
;;(global-set-key (kbd "S-SPC") 'evil-scroll-page-up)
(global-set-key [?\S- ] 'evil-scroll-page-up)

;; EXPERIMENTAL: Save current buffer and close but don't close Emacs on :wq
(evil-define-key nil evil-ex-map "wq" #'(lambda ()
                                          (interactive)
                                          (save-current-buffer)
                                          (kill-current-buffer)))
(evil-define-key nil evil-ex-map "q" #'(lambda ()
                                         (interactive)
                                         (save-current-buffer)
                                         (kill-current-buffer)))

(defun 0xMF/settings/vertico ()
  "My setup for vertico-mode."
  (interactive)
  (dolist (map (list vertico-map))
    (local-unset-key (kbd  "C-<return>"))
    (define-key map [tab] 'vertico-insert)
    (define-key map (kbd "C-l") 'vertico-insert)
    (define-key map (kbd "<return>") 'vertico-exit)
    (define-key map (kbd "C-<return>") 'vertico-exit)
    (define-key map (kbd "M-<return>") 'vertico-exit)))
(add-hook 'vertico-mode-hook '0xMF/settings/vertico)

;; Credit: [StackOverflow] in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
(defun save-this-word ()
  "Save word to personal dict."
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (flyspell-do-correct 'save nil (car word) current-location
                           (cadr word) (caddr word) current-location))))
;; ---
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active,just deactivate
it; then it takes a second \\[keyboard-quit] to abort the
minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
      (when (get-buffer "*Completions*")
        (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

;; 2 spaces for tabs
(setq-default tab-width 2 indent-tabs-mode nil)
(setq-default c-basic-offset 2 c-default-style "bsd")
(setq tab-width 2
      tab-stop-list (number-sequence 2 20 2)
      indent-line-function 'tab-to-tab-stop)

;; no backups
(setq make-backup-files nil)

;; important for markdown, GFM export, and viewing pdfs
(eval-after-load "org" '(require 'ox-md nil t))
(eval-after-load "org" '(require 'ox-gfm nil t))
(eval-after-load "org" '(require 'org-pdftools))
(eval-after-load "org" '(require 'htmlize))

(require 'use-package)
(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))
(pdf-tools-install)

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))
(add-to-list 'pdf-view-mode-hook 'pdf-view-midnight-minor-mode)

(defun 0xMF/settings/eww ()
  "Enable vi-style keybindings."
  (interactive)
  (dolist (map  (list eww-mode-map))
    (define-key map (kbd "h") 'previous-char)
    (define-key map (kbd "j") 'next-line)
    (define-key map (kbd "k") 'previous-line)
    (define-key map (kbd "l") 'next-char)
    (define-key map (kbd "n") 'eww-forward-url)
    (define-key map (kbd "p") 'eww-back-url)))
(add-hook 'eww-mode-hook '0xMF/settings/eww)


;; Credit: Bozhidar Batsov
;; http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(defun 0xMF/settings/pdf-links-minor-mode ()
  "Reset f keybinding from 'pdf-links-isearch-link."
  (interactive)
  (when (boundp 'pdf-links-minor-mode)
    (dolist (map  (list pdf-links-minor-mode-map))
      (define-key map (kbd "m") '0xMF/settings/hide-mode-line-toggle)
      (define-key map (kbd "t") 'pdf-outline)
      (define-key map (kbd "f") 'pdf-view-scroll-up-or-next-page))
    (let ((oldmap (cdr (assoc 'pdf-links-minor-mode-map minor-mode-map-alist)))
          (newmap (make-sparse-keymap)))
      (set-keymap-parent newmap oldmap)
      (define-key newmap (kbd "f") 'pdf-view-scroll-up-or-next-page)
      (make-local-variable 'minor-mode-overriding-map-alist)
      (push `(pdf-links-minor-mode . ,newmap) minor-mode-overriding-map-alist))))
(add-hook 'pdf-links-minor-mode-hook '0xMF/settings/pdf-links-minor-mode)

(defun 0xMF/settings/pdf-view ()
  "Disable blinking in pdf-view-mode and enable vi-style keybindings."
  (interactive)
  (0xMF/settings/pdf-links-minor-mode)
  (evil-set-initial-state 'pdf-view-mode 'emacs)
  (setq blink-cursor-mode nil)
  (turn-off-evil-mode)
  (setq pdf-view-continuous 't)
  (hide-mode-line-mode)
  (set (make-local-variable 'evil-emacs-state-cursor) (list nil))
  (set (make-local-variable 'evil-evilified-state-cursor) (list nil))
  (0xMF/settings/pdf-links-minor-mode)
  (local-unset-key (kbd  "C-n"))
  (local-unset-key (kbd  "C-p"))
  (local-set-key (kbd  "C-n") 'next-buffer)
  (local-set-key (kbd  "C-p") 'previous-buffer)
  (local-unset-key (kbd "b"))
  (local-set-key (kbd "b") 'pdf-view-scroll-down-or-previous-page)
  (local-set-key (kbd "f") 'pdf-view-scroll-up-or-next-page)
  (local-set-key (kbd "j") 'pdf-view-next-line-or-next-page)
  (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (local-unset-key (kbd "m"))
  (local-set-key (kbd "m") '0xMF/settings/hide-mode-line-toggle)
  (local-unset-key (kbd "n"))
  (local-set-key (kbd "n") 'pdf-view-scroll-up-or-next-page)
  (local-unset-key (kbd "p"))
  (local-set-key (kbd "p") 'pdf-view-scroll-down-or-previous-page)
  (local-set-key (kbd "J") 'pdf-view-next-page)
  (local-set-key (kbd "K") 'pdf-view-previous-page)
  (local-set-key (kbd "g") 'pdf-view-goto-page)
  (local-set-key (kbd "G") 'pdf-view-goto-page)
  (local-unset-key (kbd "h"))
  (local-set-key (kbd "h") 'image-backward-hscroll)
  (local-unset-key (kbd "l"))
  (local-set-key (kbd "l") 'image-forward-hscroll)
  (local-set-key (kbd "t") 'pdf-outline)
  (local-set-key (kbd "W") 'pdf-view-fit-width-to-window)
  (local-set-key (kbd "w") 'delete-other-windows)
  (local-set-key (kbd "H") 'pdf-view-fit-height-to-window)
  (local-set-key (kbd "P") 'pdf-view-fit-page-to-window)
  (local-set-key (kbd "/") 'isearch-forward)
  (local-set-key (kbd "?") 'isearch-backward)
  (local-set-key (kbd "<escape>")
                 #'(lambda ()
                     (interactive)
                     (kill-this-buffer)
                     (unless (one-window-p)
                       (delete-window))))
  (local-set-key (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
  (local-set-key (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page)
  (dolist (map  (list pdf-view-mode-map))
    (define-key map (kbd "b") 'pdf-view-scroll-down-or-previous-page)
    (define-key map (kbd "f") 'pdf-view-scroll-up-or-next-page)
    (define-key map (kbd "j") 'pdf-view-next-line-or-next-page)
    (define-key map (kbd "k") 'pdf-view-previous-line-or-previous-page)
    (define-key map (kbd "m") '0xMF/settings/hide-mode-line-toggle)
    (define-key map (kbd "n") 'pdf-view-scroll-up-or-next-page)
    (define-key map (kbd "p") 'pdf-view-scroll-down-or-previous-page)
    (define-key map (kbd "J") 'pdf-view-next-page)
    (define-key map (kbd "K") 'pdf-view-previous-page)
    (define-key map (kbd "g") 'pdf-view-goto-page)
    (define-key map (kbd "G") 'pdf-view-goto-page)
    (define-key map (kbd "h") 'image-backward-hscroll)
    (define-key map (kbd "l") 'image-forward-hscroll)
    (define-key map (kbd "t") 'pdf-outline)
    (define-key map (kbd "W") 'pdf-view-fit-width-to-window)
    (define-key map (kbd "w") 'delete-other-windows)
    (define-key map (kbd "H") 'pdf-view-fit-height-to-window)
    (define-key map (kbd "P") 'pdf-view-fit-page-to-window)
    (define-key map (kbd "/") 'isearch-forward)
    (define-key map (kbd "?") 'isearch-backward)
    (define-key map (kbd "<escape>")
                #'(lambda ()
                    (interactive)
                    (kill-this-buffer)
                    (unless (one-window-p)
                      (delete-window))))
    (define-key map (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
    (define-key map (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page))
  (when (boundp 'pdf-history-minor-mode-map)
    (dolist (map  (list pdf-history-minor-mode-map))
      (local-unset-key (kbd "l"))
      (define-key map (kbd "l") 'image-forward-hscroll)))
    (internal-show-cursor nil nil)
  (local-set-key (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
  (local-set-key (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page)
  (dolist (map  (list pdf-view-mode-map))
  (define-key map (kbd "b") 'pdf-view-scroll-down-or-previous-page)
  (define-key map (kbd "f") 'pdf-view-scroll-up-or-next-page)
  (define-key map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key map (kbd "m") '0xMF/settings/hide-mode-line-toggle)
  (define-key map (kbd "n") 'pdf-view-scroll-up-or-next-page)
  (define-key map (kbd "p") 'pdf-view-scroll-down-or-previous-page)
  (define-key map (kbd "J") 'pdf-view-next-page)
  (define-key map (kbd "K") 'pdf-view-previous-page)
  (define-key map (kbd "g") 'pdf-view-goto-page)
  (define-key map (kbd "G") 'pdf-view-goto-page)
  (define-key map (kbd "h") 'pdf-view-previous-page)
  (define-key map (kbd "l") 'pdf-view-next-page)
  (define-key map (kbd "t") 'pdf-outline)
  (define-key map (kbd "W") 'pdf-view-fit-width-to-window)
  (define-key map (kbd "w") 'delete-other-windows)
  (define-key map (kbd "H") 'pdf-view-fit-height-to-window)
  (define-key map (kbd "P") 'pdf-view-fit-page-to-window)
  (define-key map (kbd "/") 'isearch-forward)
  (define-key map (kbd "?") 'isearch-backward)
  (define-key map (kbd "<escape>")
                 #'(lambda ()
                    (interactive)
                    (kill-this-buffer)
                    (unless (one-window-p)
                      (delete-window))))
  (define-key map (kbd "<mouse-5>") 'pdf-view-next-line-or-next-page)
  (define-key map (kbd "<mouse-4>") 'pdf-view-previous-line-or-previous-page))
  (internal-show-cursor nil nil))
(add-hook 'pdf-view-mode-hook '0xMF/settings/pdf-view)

;;----------------------------------------------------------------------------
;; Language mode settings
;;----------------------------------------------------------------------------

(use-package markdown-mode)
:init
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-hook 'markdown-mode-hook #'(lambda () (interactive) (set-fill-column 81)))

(unless (version< emacs-version "27")
  (setq url-http-referer 'nil))

;; elm-mode
(setq elm-interactive-command '("elm" "repl")
      elm-reactor-command '("elm" "reactor")
      elm-reactor-arguments '("--port" "8000")
      elm-compile-command '("elm" "make")
      elm-compile-arguments '("--output=elm.js" "--debug")
      elm-package-command '("elm" "package"))

;;----------------------------------------------------------------------------
;; Other misc. yet imp stuff goes here. Credit: technomancy/better-defaults
;;----------------------------------------------------------------------------
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      apropos-sort-by-scores t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;----------------------------------------------------------------------------
;; Abbreviations
;;----------------------------------------------------------------------------
(setq-default abbrev-mode t)
(read-abbrev-file "~/.abbrev_defs")
(setq save-abbrevs t)

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------
;;
;; C-h-b: to check keybinding and which functions are bound to which keys
;; C-h-k: to check which key is bound to which function
;; C-h-m: to list current major mode's keys
;; C-g:   to close that opened Bindings window
;; checkout: http://ergoemacs.org/emacs/keyboard_shortcuts.html

(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-;") ctl-x-map)
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "C-j") (kbd "C-c")) ; maps one key to another
(global-set-key (kbd "M-s") 'execute-extended-command)
(global-set-key (kbd "M-z") 'execute-extended-command)
(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)

(global-set-key (kbd "C-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-;") 'evil-mode)

(global-set-key (kbd "C-M-j") 'list-buffers)
(global-set-key (kbd "C-M-h") 'previous-buffer)
(global-set-key (kbd "C-M-k") 'kill-some-buffers)
(global-set-key (kbd "C-M-l") 'next-buffer)
(global-set-key (kbd "C-M-<") 'previous-buffer)
(global-set-key (kbd "C-M->") 'next-buffer)
(global-set-key (kbd "C-M-<left>") 'previous-buffer)
(global-set-key (kbd "C-M-<up>") 'previous-buffer)
(global-set-key (kbd "C-M-s-<up>") 'previous-buffer)
(global-set-key (kbd "C-M-<prior>") 'previous-buffer)
(global-set-key (kbd "C-M-<right>") 'next-buffer)
(global-set-key (kbd "C-M-s-<right>") 'previous-buffer)
(global-set-key (kbd "C-M-<down>") 'next-buffer)
(global-set-key (kbd "C-M-s-<down>") 'next-buffer)
(global-set-key (kbd "C-M-<next>") 'next-buffer)
(global-set-key (kbd "C-M-s-<left>") 'next-buffer)
(global-set-key (kbd "C-M-SPC") 'delete-other-windows)
(global-set-key (kbd "C-M-RET") 'org-insert-heading)
(global-set-key (kbd "C-M-<return>") 'org-insert-heading)
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)
(global-set-key (kbd "C-<prior>") 'previous-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;(global-set-key (kbd "<tab>") 'tab-to-tab-stop)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(global-set-key (kbd "C-s") 'save-buffer)
;;(global-set-key (kbd "C-d") 'save-buffer)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "M-[") 'insert-pair)
(global-set-key (kbd "M-{") '0xMF/insert-braces)
(global-set-key (kbd "M-\"") 'insert-pair)

(global-set-key (kbd "C-h r") 'info-display-manual)
(global-set-key (kbd "C-h R") 'info-emacs-manual)

;;----------------------------------------------------------------------------
;; Org mode settings
;;----------------------------------------------------------------------------
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
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
(require 'org-bullets)
(require 'org-tempo)

(defun kill-misc-buffers()
  "Permanently remove some buffers."
  ;; (if (get-buffer "*scratch*")
  ;;  (kill-buffer "*scratch*"))
  (if (get-buffer "*reg group-leader*")
      (kill-buffer "*reg group-leader*")))
(add-hook 'after-change-major-mode-hook 'kill-misc-buffers)

(evil-define-key 'insert org-mode-map (kbd "C-<tab>") #'tab-to-tab-stop)

;; do not ask before prompting
(setq org-confirm-babel-evaluate nil)

;; output htmlize as css
;; refer: https://github.com/gongzhitaao/orgcss
(setq org-html-htmlize-output-type 'css)

(setq-default major-mode 'org-mode)

;;----------------------------------------------------------------------------
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
;;(require 'org-gcal)
(require 'yafolding)

(add-hook 'prog-mode-hook
          #'(lambda () (interactive) (yafolding-mode)))

(set-default 'truncate-lines t)

;; Do not ceate backups.
(setq  make-backup-files nil)

(defun load-if-file-exists (FILE)
  "Check if FILE exists before loading it."
  (if (file-readable-p FILE)
      (load-file FILE)))

(load-if-file-exists (expand-file-name "~/.emacs.d/lisp/secrets.el"))

(menu-bar-mode -1)

;; Using mouse to select and copy text to the clipboard
;; Source: [StackOverflow] how-to-combine-emacs-primary-clipboard-copy-and-paste-behavior-on-ms-windows
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;;

;; Set default font
;; (used and saved through menu Options->Set Default Font... into cutom.el)
;; optionally (set-frame-font "Source Code Pro Semibold-10")
(cond
 ((member "Source Code Pro" (font-family-list))
  (set-frame-font "Source Code Pro-13:style=Semibold" nil t))
 ((member "Source Code Variable" (font-family-list))
  (set-frame-font "Source Code Variable-13:style=Semibold" nil t))
 ((member "DejaVu Sans Mono" (font-family-list))
  (set-frame-font "DejaVu Sans Mono-10")))


(unless (version<= emacs-version "25")
  (require 'fill-column-indicator))

;;(benchmark-init/show-durations-tabulated)
;; show battery indicator on mode
(display-battery-mode t)

;; hide trailing whitespace in command output from showing up in eshell
(add-hook 'eshell-mode-hook
          (defun hide-trailing-whitespace ()
            (interactive)
            (setq show-trailing-whitespace nil)))

(add-hook 'Info-mode-hook
          (defun 0xMF/settings/Info-mode ()
            "Enable vi-style keybindings."
            (interactive)
            (turn-off-evil-mode)
            (setq Info-use-header-line nil)
            (local-set-key (kbd "g") 'beginning-of-buffer)
            (local-set-key (kbd "G") 'end-of-buffer)
            (local-set-key (kbd "h") 'left-char)
            (local-set-key (kbd "J") 'next-line)
            (local-set-key (kbd "j") 'pixel-scroll-up)
            (local-set-key [down] 'pixel-scroll-up)
            (local-set-key [up] 'pixel-scroll-down)
            (local-set-key (kbd "k") 'pixel-scroll-down)
            (local-set-key (kbd "K") 'previous-line)
            (local-set-key (kbd "l") 'right-char)
            (local-set-key (kbd "<return>") 'Info-follow-nearest-node)
            (dolist (map  (list Info-mode-map))
              (define-key map (kbd "m") 'Info-menu)
              (define-key map (kbd "n") 'Info-forward-node)
              (define-key map (kbd "p") 'Info-backward-node)
              (define-key map (kbd "q") 'delete-other-windows))
            (message "0xMF/settings/Info-mode")))

(defun 0xMF/settings/hide-mode-line-toggle ()
  "Toggle mode line toggle."
  (interactive)
  (hide-mode-line-mode (if hide-mode-line-mode -1 +1))
  (unless hide-mode-line-mode
    (redraw-display)))

(setq counsel-find-file-ignore-regexp (concat "\\(.~undo-tree~\\|"
                                              ".desktop\\|"
                                              ".git\\|"
                                              ".historian\\|"
                                              ".lock\\|"
                                              ".*.fasl\\|"
                                              ".*~\\|"
                                              "#*#\\)"))

(defun 0xMF/toggle-browser-eww ()
  "Toggle between eww and chromium as default browser for html files."
  (interactive)
  (setq browse-url-browser-function
        (if (equal browse-url-browser-function 'eww-browse-url)
            'browse-url-chromium
            'eww-browse-url))
  (message "setting browser to '%s'" browse-url-browser-function))

(setq 0xMF/toggle-font-large-normal nil)
(defun 0xMF/toggle-font-large-normal ()
  "Toggle font sizes between large/normal."
  (interactive)
  (if (get '0xMF/toggle-font-large-normal 'state)
      (progn
        (set-frame-font "Source Code Pro-13:style=Semibold" nil t)
        (put '0xMF/toggle-font-large-normal 'state nil))
    (progn
      (set-frame-font "Source Code Pro-15:style=Semibold" nil t)
      (put '0xMF/toggle-font-large-normal 'state t)))
  (message 0xMF/toggle-font-large-normal))

(defun 0xMF/normal-font ()
  "Increase font size."
  (interactive)
  (when (member "Source Code Pro" (font-family-list))
    (set-frame-font "Source Code Pro-13:style=Semibold" nil t)))

;; ----------------------------------
;; Calendar setup
;; avoid evil keybindings in these modes by default
(require 'calendar)

(add-to-list 'evil-emacs-state-modes 'calendar-mode 'package-menu-mode)
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-set-initial-state 'pdf-view-mode 'emacs)
(add-hook 'pdf-view-mode-hook
          #'(lambda ()
              (interactive)
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))


(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
Default does not OMIT-DAY-OF-WEEK-P.
With a prefix argument,the date is inserted without the day of the week."
  (interactive "P*")
  (insert (calendar-date-string (calendar-current-date) nil
                                omit-day-of-week-p)))

(defun 0xMF/settings/calendar-mode ()
  "My calendar mode settings."
  (interactive)
  ;;(local-set-key (kbd "i") 'diary-insert-entry)
  (message "added settings for calendar-mode"))

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

(defun 0xMF/settings/orgmode ()
"My Org+Evil settings.
Turn on spell check automatically; maketext wrap at 81; and make
'org-mode' default for scratch (new) buffers."
(interactive)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message
      (concat "# Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))
(org-bullets-mode 1)
(evil-define-key 'normal org-mode-map [tab] #'org-cycle)
(evil-define-key 'normal org-mode-map (kbd "S-TAB") #'org-shifttab)
(turn-on-flyspell)
(set-fill-column 81))
(add-hook 'org-mode-hook '0xMF/settings/orgmode)

(defun 0xMF/settings/textmode ()
  "Wrap lines (hard return) around column 81."
  (interactive)
  (menu-bar--toggle-truncate-long-lines)
  (turn-off-auto-fill)
  (set-fill-column 81)
  (turn-on-visual-line-mode))
(add-hook 'text-mode-hook '0xMF/settings/textmode)

(defun 0xMF/settings/wrap-line-length (textwidth)
  "Wrap lines (hard return) around TEXTWIDTH."
  (interactive "nEnter text width for wrapping: ")
  (set-fill-column textwidth)
  (turn-on-auto-fill))

(defun 0xMF/orgmode-remove-tag (tag)
  "Remove TAG from line."
  (interactive "sTag:")
  (org-toggle-tag tag 'off))

(defun 0xMF/settings/package-menu-mode ()
  "My settings for package menu."
  (define-key package-menu-mode-map (kbd "; s") '0xMF/startup)
  (define-key package-menu-mode-map (kbd "/ n") nil )
  (define-key package-menu-mode-map (kbd "/ j") 'package-menu-filter-by-name))

(defun 0xMF/insert-braces ()
  "Source: stackoverflow.com/questions/2951797/wrapping-selecting-text-in-enclosing-characters-in-emacs."
  (interactive)
  (if (region-active-p)
      (insert-pair 1 ?{ ?})
      (insert "{}")
      (backward-char)))

(defun 0xMF/kill-some-buffers (regexp)
  "Kill buffers matching REGEXP without confirmation."
  (interactive "Kill buffers matching the regex given: ")
  (cl-letf (((symbol-function 'kill-buffer-ask)
             #'(lambda (buffer) (kill-buffer buffer))))
    (kill-matching-buffers regexp)))

(defvar 0xMF/kill-all-magit t "Removes all magit-buffers (inucluding magit process).")

(defun 0xMF/startup ()
  "Start/reset Emacs the way like it ;-)."
  (interactive)
  (global-display-line-numbers-mode -1)
  (display-line-numbers-mode -1)
  (line-number-mode t)
  (org-toggle-pretty-entities)
  (when (equal major-mode 'org-mode)
    (org-set-visibility-according-to-property)
    (setq electric-pair-mode nil))
  (when (equal major-mode 'Info-mode)
    (0xMF/settings/Info-mode))
  (when (fboundp 'ivy-minibuffer-map)
    (0xMF/settings/ivy-minibuffer))
  (when (fboundp '0xMF/local)
    (0xMF/local))
  (0xMF/settings/vi)
  (message "0xMF/startup"))

(defun 0xMF/wrap ()
  "Toggle line wrapping."
  (interactive)
  (toggle-truncate-lines))

(defun 0xMF/shrink ()
  "Shrink table according to cookie at point."
  (interactive)
  (org-table-shrink))

(defun 0xMF/zero ()
  "Put Emacs into distraction free mode."
  (interactive)
  (0xMF/kill-some-buffers "^\\*Fancy Diary Entries*")
  (0xMF/kill-some-buffers "^\\timelog")
  (0xMF/kill-some-buffers "^\\*Info*")
  (0xMF/startup)
  (message "0xMF/zero"))

(add-hook 'after-init-hook '0xMF/startup)

(defun 0xMF/evil-magit ()
  "Enable evil-magit support."
  (interactive)
  (require 'evil-magit)
  (evil-magit-init)
  (setq evil-magit-state 'motion))

(defun file-reload ()
  "Reload file without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-0xMF-evil)
;;; init-0xMF-evil.el ends here.
