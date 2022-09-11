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
(defun 0xMF/settings/ivy-minibuffer ()
  "Bring sanity back to up/down keybindings."
  (interactive)
  (dolist (map  (list ivy-minibuffer-map))
    (define-key map [up] 'ivy-previous-line)))
(add-hook 'ivy-minibuffer-hook '0xMF/settings/ivy-minibuffer)
(add-hook 'ivy-mode-hook '0xMF/settings/ivy-minibuffer)

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

(defun 0xMF/cleanup-Emacs-buffer-list ()
  "Remove all kinds of needless buffers."
  (0xMF/kill-some-buffers "^\\Diary")
  (0xMF/kill-some-buffers "^\\*Aprops*")
  (0xMF/kill-some-buffers "^\\*Backtrace*")
  (0xMF/kill-some-buffers "^\\*Buffer List*")
  (0xMF/kill-some-buffers "^\\*Calculator*")
  (0xMF/kill-some-buffers "^\\*Calendar*")
  (0xMF/kill-some-buffers "^\\*Command Line*")
  (when (get-buffer "*Compile-Log*")
    (kill-buffer "*Compile-Log*"))
  (0xMF/kill-some-buffers "^\\*Ediff Registry*")
  (0xMF/kill-some-buffers "^\\*Flycheck error messages*")
  (0xMF/kill-some-buffers "^\\*Flymake log*")
  (0xMF/kill-some-buffers "^\\*Help*")
  (0xMF/kill-some-buffers "^\\*HS-Error*")
  (0xMF/kill-some-buffers "^\\*List of Slides*")
  (0xMF/kill-some-buffers "^\\*Org-Babel Error Output*")
  (0xMF/kill-some-buffers "^\\*Org PDF Latex Output*")
  (0xMF/kill-some-buffers "^\\*Packages*")
  (0xMF/kill-some-buffers "^\\*PP Eval Output*")
  (0xMF/kill-some-buffers "^\\*Outline ")
  (0xMF/kill-some-buffers "^\\*WoMan-Log*")
  (0xMF/kill-some-buffers "^\\*Warnings*")
  (0xMF/kill-some-buffers "^\\*cabal")
  (0xMF/kill-some-buffers "^\\*compilation*")
  (0xMF/kill-some-buffers "^\\*dante:")
  (mapc #'(lambda (buffer)
            (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
              (kill-buffer buffer))) (buffer-list))
  (0xMF/kill-some-buffers "^\\*eldoc*")
  (0xMF/kill-some-buffers "^\\*hs-lint*")
  (when (bound-and-true-p 0xMF/kill-all-magit)
    (0xMF/kill-some-buffers "^magit:")
    (0xMF/kill-some-buffers "^magit-diff:")
    (0xMF/kill-some-buffers "^magit-log:")
    (0xMF/kill-some-buffers "^magit-merge-preview:")
    (0xMF/kill-some-buffers "^magit-process:")
    (0xMF/kill-some-buffers "^magit-revision:")
    (0xMF/kill-some-buffers "^\\*magit-todos--scan-with-git-grep"))
  (0xMF/kill-some-buffers "^popup-win-dummy")
  (0xMF/kill-some-buffers "^\\*vc-diff*")
  (0xMF/settings/orgmode)
  (get-buffer-create "*scratch*"))

(defun 0xMF/startup ()
  "Start/reset Emacs the way like it ;-)."
  (interactive)
  (0xMF/cleanup-Emacs-buffer-list)
  (global-display-line-numbers-mode -1)
  (display-line-numbers-mode -1)
  (line-number-mode t)
  (org-toggle-pretty-entities)
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

(defun 0xMF/vi ()
  "Reset/set settings to vim."
  (interactive)
  (turn-on-evil-mode)
  (toggle-truncate-lines))

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
