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

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------
;;

;;(define-key input-decode-map "\e[6;5~" [C-down])
;;(define-key input-decode-map "\e[5;5~" [C-up])

;; C-h-b: to check keybinding and which functions are bound to which keys
;; C-h-k: to check which key is bound to which function
;; C-h-m: to list current major mode's keys
;; C-g:   to close that opened Bindings window
;; checkout: http://ergoemacs.org/emacs/keyboard_shortcuts.html

;;(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "M-z") nil)
(global-set-key (kbd "C-;") ctl-x-map)
(global-set-key (kbd "C-z") ctl-x-map)
;;(global-set-key (kbd "C-j") nil)
;;(global-set-key (kbd "C-j") (kbd "C-c")) ; maps one key to another
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
(global-set-key (kbd "C-<prior>") 'scroll-up-command)
(global-set-key (kbd "C-<next>") 'scroll-down-command)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(global-set-key (kbd "<tab>") 'tab-to-tab-stop)

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;;(global-set-key (kbd "C-s") 'save-buffer)
;;(global-set-key (kbd "C-d") 'save-buffer)
;;(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; (global-set-key (kbd "M-[") 'insert-pair)
;; (global-set-key (kbd "M-{") '0xMF/insert-braces)
;; (global-set-key (kbd "M-\"") 'insert-pair)

(global-set-key (kbd "C-h r") 'info-display-manual)
(global-set-key (kbd "C-h R") 'info-emacs-manual)


;;----------------------------------------------------------------------------
;; Evil mode settings
;;----------------------------------------------------------------------------
(defun 0xMF/settings/cursor-emacs-evil-toggle()
  "Cursor color indicates mode: white = Emacs, green = evil (Vi/Vim)."
  (interactive)
  (when (display-graphic-p)
    (if (string= (symbol-value 'evil-state) "normal")
        (set-cursor-color "green")
      (set-cursor-color "white"))))

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
  (if (fboundp pdf-view-display-size)
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

(evil-define-key 'insert org-mode-map (kbd "C-<tab>") #'tab-to-tab-stop)

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
    (define-key map (kbd "C-f") 'scroll-up-command)
    (define-key map (kbd "C-b") 'scroll-down-command)
    (define-key map (kbd "C-j") #'(lambda () (interactive) (evil-scroll-down nil)))
    (define-key map (kbd "C-k") #'(lambda () (interactive) (evil-scroll-up nil)))
    (local-unset-key (kbd "SPC"))
    (define-key map (kbd "SPC") 'scroll-up-command)
    (local-unset-key (kbd "M-SPC"))
    (define-key map (kbd "M-SPC") 'scroll-down-command)
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

(when (fboundp 'company-mode-map)
  (define-key company-active-map [tab] 'company-complete-common))
                                        ;(dolist (map  (list company-active-map))
                                        ;  (define-key map (kbd "<tab>") 'company-complete-common))

(dolist (map  (list minibuffer-local-isearch-map))
  (define-key map (kbd "n") 'isearch-printing-char))

(global-set-key [escape] 'evil-exit-emacs-state)

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

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-0xMF-evil)
;;; init-0xMF-evil.el ends here.
