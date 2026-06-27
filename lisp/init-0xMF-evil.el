;;; init-0xMF-evil.el --  -*- lexical-binding: t -*-
;;; -----------------
;;
;;; package: override Purcell's Emacs with my evil preferences
;;
;;; Commentary:
;;
;;  all my Vi preferences and customizations are here.

;;; Code:

(require 'evil)
(require 'general)
(require 'undo-fu)
(evil-mode 1)

(setq evil-default-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-normal-state-cursor '("green" box))
(setq evil-operator-state-cursor '("red" hollow))
(setq evil-replace-state-cursor '("red" box))
(setq evil-visual-state-cursor '("orange" box))

;; better clipboard copy-paste with evil
(fset 'evil-visual-update-x-selection 'ignore)

;;----------------------------------------------------------------------------
;; Evil state modes setup
;;----------------------------------------------------------------------------
(require 'calendar)
(dolist (mode '(calendar-mode package-menu-mode))
  (add-to-list 'evil-emacs-state-modes mode))
(evil-set-initial-state 'calendar-mode 'emacs)
(evil-set-initial-state 'package-menu-mode 'emacs)
(evil-set-initial-state 'pdf-view-mode 'emacs)
(add-hook 'pdf-view-mode-hook
          #'(lambda ()
              (interactive)
              (set (make-local-variable 'evil-emacs-state-cursor) (list nil))))

;;----------------------------------------------------------------------------
;; General keymap settings using general.el
;;----------------------------------------------------------------------------

;; bind a key globally in normal state
(setq general-default-keymaps 'evil-normal-state-map)

;; bind j and k in normal state globally
(general-define-key "j" 'evil-next-visual-line
                    "k" 'evil-previous-visual-line
                    "SPC" 'evil-scroll-page-down
                    "DEL" 'evil-scroll-page-up)

;; bind wm and wc
(general-define-key :prefix "w"
                    "a" 'org-toggle-link-display ; 'beginning-of-line
                    "c" 'whitespace-cleanup
                    "d" #'(lambda ()
                            (interactive)
                            (kill-buffer)
                            (unless (one-window-p)
                              (delete-window)))
                    "e" 'end-of-line
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
                    "o" 'other-window
                    "O" #'(lambda ()
                            (interactive)
                            (other-window 1)
                            (unless (one-window-p)
                              (delete-other-windows)))
                    "p" 'previous-buffer
                    "r" 'evil-window-rotate-upwards
                    "R" 'evil-window-rotate-downwards
                    "u" 'winner-undo
                    "U" 'winner-redo
                    "w" 'delete-other-windows
                    "W" 'delete-window
                    "0" 'delete-window
                    "1" 'delete-window)

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

;; named prefix key allows ; to be used a mapper for my keybindings
(setq 0xMF-leader1 ";")
(general-define-key :prefix 0xMF-leader1
                    "a" '0xMF/settings/orgmode-emphasis-markers-toggle
                    "A" 'org-agenda
                    "B" 'switch-to-buffer
                    "c" '0xMF/settings/theme
                    "C" 'comment-region
                    "d" 'insdate-insert-current-date
                    "D" 'org-agenda-list
                    "e" 'eval-last-sexp
                    "E" 'eval-region
                    "f" 'set-fill-column
                    "F" 'fill-paragraph
                    "g" '0xMF/settings/cursor-column-toggle
                    "h" 'hl-line-mode
                    "i" '0xMF/settings/Info-mode
                    "k" 'kill-this-buffer
                    "l" 'whitespace-mode
                    "L" 'org-open-at-point
                    "m" 'magit-mode
                    "n" 'display-line-numbers-mode
                    "o" 'find-file
                    "O" 'org-open-at-point
                    "p" '0xMF/settings/theme
                    "P" '0xMF/settings/show-cursor-toggle
                    "q" 'toggle-truncate-lines
                    "r" '0xMF/reset
                    "R" 'file-reload
                    "s" '0xMF/startup
                    "T" 'org-set-tags
                    "u" 'undo-tree-undo
                    "v" '0xMF/vi
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
                    "/" 'org-tags-view
                    "." 'org-tags-view
                    "\\" 'org-match-sparse-tree)

;; Bind keys in multiple states of Org-mode
(general-define-key :keymaps 'org-mode-map
                    :states '(insert emacs)
                    "<tab>" 'org-cycle)

;;----------------------------------------------------------------------------
;; Cursor and display toggle settings
;;----------------------------------------------------------------------------
(defun 0xMF/settings/cursor-emacs-evil-toggle()
     "Cursor color indicates mode: white = Emacs, green = evil (Vi/Vim)."
     (interactive)
     (when (display-graphic-p)
       (if (string= (symbol-value 'evil-state) "normal")
           (set-cursor-color "green")
         (set-cursor-color "white"))))
(add-hook 'evil-mode-hook '0xMF/settings/cursor-emacs-evil-toggle)

(defun 0xMF/settings/cursor-column-toggle()
  "Show and follow cursor column."
  (interactive)
  (vline-mode 'toggle)
  (if (bound-and-true-p vline-face)
      (progn
        (if (string= (face-background 'default) "#f5f5dc")
            (set-face-background vline-face "firebrick")
          (set-face-background vline-face "light steel blue")))))

(defun 0xMF/settings/cursor-toggle ()
  "Toggle showing cursor."
  (interactive)
  (if (fboundp 'pdf-view-display-size)
      (internal-show-cursor nil nil))
  (if (internal-show-cursor-p)
      (internal-show-cursor nil nil)
    (internal-show-cursor nil t)))

;;----------------------------------------------------------------------------
;; Lisp Repl settings (slime and sly)
;;----------------------------------------------------------------------------
(defun 0xMF/settings/slime ()
  "My slime settings."
  (interactive)
  (turn-on-evil-mode)
  (dolist (map (list slime-repl-mode-map))
    (local-unset-key (kbd  "C-<return>"))
    (define-key map (kbd "C-<return>") 'slime-repl-newline-and-indent)))

(defun 0xMF/settings/sly ()
  "My sly settings."
  (interactive)
  (turn-on-evil-mode)
  (dolist (map (list sly-repl-mode-map))
    (local-unset-key (kbd  "C-<return>"))
    (define-key map (kbd "C-<return>") 'sly-repl-newline-and-indent)))

;;----------------------------------------------------------------------------
;; Ivy and packages hook
;;----------------------------------------------------------------------------
(defun 0xMF/settings/ivy-minibuffer ()
  "Bring sanity back to up/down keybindings."
  (interactive)
  (dolist (map  (list ivy-minibuffer-map))
    (define-key map [up] 'ivy-previous-line)))
(add-hook 'ivy-minibuffer-hook '0xMF/settings/ivy-minibuffer)
(add-hook 'ivy-mode-hook '0xMF/settings/ivy-minibuffer)

;;----------------------------------------------------------------------------
;; Language mode Evil settings
;;----------------------------------------------------------------------------
(require 'haskell-mode)
(require 'hindent)

(defun 0xMF/settings/haskell-mode ()
  "Override some evil-mode settings when in haskell-mode."
  (interactive)
  (evil-local-set-key 'normal (kbd "; q") 'hindent-reformat-decl-or-fill)
  (message "0xMF/settings/haskell-mode: ;-q"))
(with-eval-after-load 'haskell-mode
  (add-hook 'haskell-mode-hook '0xMF/settings/haskell-mode))

(defun 0xMF/settings/evil-orgmode ()
  "My Org+Evil settings."
  (interactive)
  (when (fboundp 'evil-mode)
    (evil-define-key 'normal org-mode-map [tab] #'org-cycle)
    (evil-define-key 'normal org-mode-map (kbd "S-TAB") #'org-shifttab)))
(add-hook 'org-mode-hook '0xMF/settings/evil-orgmode)

;;----------------------------------------------------------------------------
;; Consolidated Vi/Vim settings function
;;----------------------------------------------------------------------------
(defun 0xMF/settings/vi ()
  "My Vi settings."
  (interactive)
  (turn-on-evil-mode)
  (local-unset-key (kbd "C-e"))
  (local-unset-key (kbd "C-j"))
  (local-unset-key (kbd "C-k"))
  (local-unset-key (kbd "C-n"))
  (local-unset-key (kbd "C-p"))
  (dolist (map (list evil-insert-state-map))
    (define-key map (kbd "C-a") 'beginning-of-line)
    (define-key map (kbd "C-e") 'end-of-line)
    (define-key map (kbd "C-j") 'next-line)
    (define-key map (kbd "C-k") 'previous-line)
    (define-key map (kbd "C-n") 'next-buffer)
    (define-key map (kbd "C-p") 'previous-buffer))
  (dolist (map (list evil-normal-state-map))
    (define-key map (kbd "b") 'evil-backward-word-begin)
    (define-key map (kbd "j") 'evil-next-visual-line)
    (define-key map (kbd "k") 'evil-previous-visual-line)
    (define-key map (kbd "p") 'evil-paste-after)
    (define-key map (kbd "q") #'(lambda () (interactive) (unless (one-window-p) (delete-other-windows)) (keyboard-quit)))
    (define-key map (kbd "u") 'undo)
    (define-key map [escape] #'(lambda () (interactive) (unless (one-window-p) (delete-other-windows)) (keyboard-quit)))
    (define-key map [prior] 'evil-scroll-page-up)
    (define-key map [next] 'evil-scroll-page-down)
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    (define-key map (kbd "C-e") 'end-of-line)
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "C-j") 'next-line)
    (define-key map (kbd "C-k") 'previous-line)
    (local-unset-key (kbd "SPC"))
    (define-key map (kbd "SPC") 'scroll-up-command)
    (local-unset-key (kbd "M-SPC"))
    (define-key map (kbd "M-SPC") 'scroll-down-command)
    (define-key map (kbd "C-d") 'save-buffer)
    (define-key map (kbd "C-n") 'next-buffer)
    (define-key map (kbd "C-p") 'previous-buffer)
    (define-key map (kbd "C-r") 'undo-fu-only-redo)
    (define-key map (kbd "M-n") 'evil-scroll-page-down)
    (define-key map (kbd "M-p") 'evil-scroll-page-up))

  (define-key minibuffer-local-map [tab] 'vertico-insert)
  (define-key minibuffer-local-map (kbd "M-<return>") 'vertico-exit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  (when (fboundp 'company-mode-map)
    (define-key company-active-map [tab] 'company-complete-common))

  (dolist (map (list minibuffer-local-isearch-map))
    (define-key map (kbd "n") 'isearch-printing-char))

  (global-set-key [escape] 'evil-exit-emacs-state)
  (global-set-key [?\S- ] 'evil-scroll-page-up)

  ;; EXPERIMENTAL: Save current buffer and close but don't close Emacs on :wq / :q
  (evil-define-key nil evil-ex-map "wq" #'(lambda ()
                                            (interactive)
                                            (save-current-buffer)
                                            (kill-current-buffer)))
  (evil-define-key nil evil-ex-map "q" #'(lambda ()
                                           (interactive)
                                           (save-current-buffer)
                                           (kill-current-buffer))))

(defun 0xMF/vi ()
  "Reset/set settings to vim."
  (interactive)
  (turn-on-evil-mode)
  (0xMF/settings/vi)
  (toggle-truncate-lines))

;;----------------------------------------------------------------------------
;; Global non-Vim keybindings overrides
;;----------------------------------------------------------------------------
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-;") ctl-x-map)
(global-set-key (kbd "C-z") ctl-x-map)
(global-set-key (kbd "M-s") 'execute-extended-command)
(global-set-key (kbd "M-z") 'execute-extended-command)
(global-set-key (kbd "C-n") 'next-buffer)
(global-set-key (kbd "C-p") 'previous-buffer)
(global-set-key (kbd "C-j") 'next-line)
(global-set-key (kbd "C-k") 'previous-line)

(global-set-key (kbd "C-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-<escape>") 'evil-mode)
(global-set-key (kbd "C-M-;") 'evil-mode)

(global-set-key (kbd "C-M-j") 'list-buffers)
(global-set-key (kbd "C-M-h") 'previous-buffer)
(global-set-key (kbd "C-M-k") 'kill-some-buffers)
(global-set-key (kbd "C-M-l") 'next-buffer)
(global-set-key (kbd "C-M-=") #'(lambda () (interactive) (text-scale-increase 1)))
(global-set-key (kbd "C-M--") #'(lambda () (interactive) (text-scale-decrease 1)))

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

;; fixes text resizing in Emacs 30.2
;; ensuring line numbers scale with buffer text (C-x C-+)
(global-set-key (kbd "C-=") #'(lambda()
                                (interactive)
                                (text-scale-increase 1)
                                (set-face-attribute 'line-number nil :family 'unspecified :height 'unspecified :inherit 'default)
                                (set-face-attribute 'line-number-current-line nil :family 'unspecified :height 'unspecified :inherit 'default)))

(global-set-key (kbd "C--") #'(lambda()
                                (interactive)
                                (text-scale-decrease 1)
                                (set-face-attribute 'line-number nil :family 'unspecified :height 'unspecified :inherit 'default)
                                (set-face-attribute 'line-number-current-line nil :family 'unspecified :height 'unspecified :inherit 'default)))

(global-set-key (kbd "C-<prior>") 'scroll-up-command)
(global-set-key (kbd "C-<next>") 'scroll-down-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-h r") 'info-display-manual)
(global-set-key (kbd "C-h R") 'info-emacs-manual)

;;----------------------------------------------------------------------------
;; Slime Lisp Helper
;;----------------------------------------------------------------------------
;; M-x slime calls sbcl
(load (expand-file-name "~/.comp.misc/lisp/quicklisp/slime-helper.el"))
(require 'slime-autoloads)
(setq inferior-lisp-program "sbcl")
(setq slime-default-lisp 'sbcl)
(setq slime-contribs '(slime-scratch slime-editing-commands slime-fancy))
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))
(put 'lambda 'lisp-indent-function 'defun)
(put 'while 'lisp-indent-function 1)
(put 'unless 'lisp-indent-function 1)
(put 'if 'lisp-indent-function nil)
(put 'do 'lisp-indent-function 2)
(put 'do* 'lisp-indent-function 2)

;;----------------------------------------------------------------------------
;; Magit evil support
;;----------------------------------------------------------------------------
(defun 0xMF/evil-magit ()
  "Enable evil-magit support."
  (interactive)
  (require 'evil-magit)
  (evil-magit-init)
  (setq evil-magit-state 'motion))

(provide 'init-0xMF-evil)
;;; init-0xMF-evil.el ends here
