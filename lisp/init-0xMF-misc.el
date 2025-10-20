;;; init-0xMF-misc.el --  -*- lexical-binding: t -*-
;;; -----------------
;;
;;; package: override Purcell's Emacs with my misc evil preferences
;;
;;; Commentary:
;;
;;  all my misc preferences and customizations are here.

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
;;(defun 0xMF/settings/pdf-links-minor-mode ()
;;  "Reset f keybinding from 'pdf-links-isearch-link."
;;  (interactive)
;;  (when (fboundp 'pdf-links-minor-mode)
;;    (dolist (map  (list pdf-links-minor-mode-map))
;;      (define-key map (kbd "m") '0xMF/settings/hide-mode-line-toggle)
;;      (define-key map (kbd "t") 'pdf-outline)
;;      (define-key map (kbd "f") 'pdf-view-scroll-up-or-next-page))
;;    (let ((oldmap (cdr (assoc 'pdf-links-minor-mode-map minor-mode-map-alist)))
;;          (newmap (make-sparse-keymap)))
;;      (set-keymap-parent newmap oldmap)
;;      (define-key newmap (kbd "f") 'pdf-view-scroll-up-or-next-page)
;;      (make-local-variable 'minor-mode-overriding-map-alist)
;;      (push `(pdf-links-minor-mode . ,newmap) minor-mode-overriding-map-alist))))
;;(add-hook 'pdf-links-minor-mode-hook '0xMF/settings/pdf-links-minor-mode)

;; Credit: Bozhidar Batsov
;; http://emacsredux.com/blog/2013/09/25/removing-key-bindings-from-minor-mode-keymaps/
(defun 0xMF/settings/pdf-links-minor-mode ()
  "Reset f keybinding from 'pdf-links-isearch-link."
  (interactive)
  (when (boundp 'pdf-links-minor-mode-map)
    (let ((oldmap (cdr (assoc 'pdf-links-minor-mode-map minor-mode-map-alist)))
          (newmap (make-sparse-keymap)))
      (set-keymap-parent newmap oldmap)
      (define-key newmap (kbd "m") '0xMF/settings/hide-mode-line-toggle)
      (define-key newmap (kbd "t") 'pdf-outline)
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
  (when (fboundp 'pdf-history-minor-mode-map)
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
;; Miscalleanous settings
;; User mode settings for UI/keyboard/look and feel
;;----------------------------------------------------------------------------
(require 'yafolding)

(add-hook 'prog-mode-hook
          #'(lambda () (interactive) (yafolding-mode)))

(set-default 'truncate-lines t)

;; Do not ceate backups.
(setq  make-backup-files nil)

(menu-bar-mode -1)

;; Using mouse to select and copy text to the clipboard
;; Source: [StackOverflow] how-to-combine-emacs-primary-clipboard-copy-and-paste-behavior-on-ms-windows
(setq select-active-regions nil)
(setq mouse-drag-copy-region t)
(global-set-key [mouse-2] 'mouse-yank-at-click)
;;

;; smooth scrolling
(setq scroll-margin 5
      scroll-conservatively 9999
      scroll-step 1)

(unless (version<= emacs-version "25")
  (require 'fill-column-indicator))

;;(benchmark-init/show-durations-tabulated)
;; show battery indicator on mode
(display-battery-mode t)

;; hide trailing whitespace in command output from showing up in eshell
(defun 0xMF/settings/hide-trailing-whitespace ()
  (interactive)
  (setq show-trailing-whitespace nil))
(add-hook 'eshell-mode-hook '0xMF/settings/hide-trailing-whitespace)

(defun 0xMF/settings/Info-mode ()
  "Enable vi-style keybindings."
  (interactive)
  (turn-off-evil-mode)
  (setq Info-use-header-line nil)
  (hide-mode-line-mode t)
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
    (define-key map (kbd "Q") 'delete-other-windows))
  (message "0xMF/settings/Info-mode"))
(add-hook 'Info-mode-hook '0xMF/settings/Info-mode)

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
  (when (fboundp 'evil-mode)
    (0xMF/settings/vi))
  (message "0xMF/startup"))

(defun 0xMF/settings/package-menu-mode ()
  "My settings for package menu."
  (define-key package-menu-mode-map (kbd "; s") '0xMF/startup)
  (define-key package-menu-mode-map (kbd "/ n") nil )
  (define-key package-menu-mode-map (kbd "/ j") 'package-menu-filter-by-name))

(defun 0xMF/wrap ()
  "Toggle line wrapping."
  (interactive)
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

(defun kill-misc-buffers()
  "Permanently remove some buffers."
  ;; (if (get-buffer "*scratch*")
  ;;  (kill-buffer "*scratch*"))
  (if (get-buffer "*reg group-leader*")
      (kill-buffer "*reg group-leader*")))
(add-hook 'after-change-major-mode-hook 'kill-misc-buffers)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'init-0xMF-misc)

;;;  -*- mode: Lisp;-*
;;; init-0xMF-misc.el ends here
