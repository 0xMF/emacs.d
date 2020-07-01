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

(quote (package-selected-packages '(add-node-modules-path
                                    aggressive-indent alert anaconda-mode anzu auto-compile avy beacon browse-at-remote
                                    browse-kill-ring bug-reference-github bundler cask-mode cider cl-lib-highlight cl-libify
                                    cljsbuild-mode clojure-mode coffee-mode color-theme-sanityinc-solarized
                                    color-theme-sanityinc-tomorrow command-log-mode company company-anaconda company-nixos-options
                                    company-php company-quickhelp company-terraform consult consult-flycheck counsel crontab-mode
                                    css-eldoc csv-mode daemons dante default-text-scale dhall-mode diff-hl diminish dimmer diredfl
                                    disable-mouse docker docker-compose-mode dockerfile-mode dotenv-mode dune dune-format elein
                                    elisp-slime-nav elm-mode elm-test-runner embark embark-consult envrc eradio erlang
                                    evil-collection evil-magit exec-path-from-shell expand-region fill-column-indicator flycheck
                                    flycheck-clojure flycheck-color-mode-line flycheck-elm flycheck-ledger flycheck-nim
                                    flycheck-package flycheck-relint flycheck-rust forge general fullframe git-blamed git-commit
                                    git-timemachine gitconfig-mode github-clone github-review gitignore-mode gnu-elpa-keyring-update
                                    gnuplot go-mode goto-line-preview haml-mode haskell-mode hide-mode-line
                                    highlight-escape-sequences highlight-quoted hindent htmlize httprepl ibuffer-projectile
                                    ibuffer-vc immortal-scratch inf-ruby info-colors ipretty ivy ivy-rich ivy-xref j-mode
                                    js-comint js2-mode json-mode ledger-mode list-unicode-display lua-mode macrostep magit
                                    magit-todos marginalia markdown-mode merlin merlin-company merlin-eldoc mmm-mode
                                    mode-line-bell move-dup multiple-cursors nginx-mode nim-mode nix-buffer nix-mode nix-sandbox
                                    nixos-options nixpkgs-fmt orderless org org-beautify-theme org-bullets org-caldav
                                    org-cliplink org-gcal org-gtd org-noter-pdftools org-pomodoro org-present
                                    org-static-blog origami ox-asciidoc page-break-lines paredit php-mode pip-requirements
                                    powerline prettier-js projectile projectile-rails psc-ide psci purescript-mode racer
                                    racket-mode rainbow-delimiters rainbow-mode reformatter regex-tool restclient robe rspec-mode
                                    ruby-compilation ruby-hash-syntax rust-mode sass-mode scratch selectrum selectrum-prescient
                                    seq session shfmt skewer-less skewer-mode slime slime-company smart-mode-line
                                    smart-mode-line-powerline-theme smarty-mode smex sqlformat ssh-agency sudo-edit swiper
                                    switch-window symbol-overlay tagedit terraform-mode textile-mode toml-mode tuareg
                                    typescript-mode undo-fu unfill uptimes use-package vc-darcs vertico vlf wgrep which-key
                                    whitespace-cleanup-mode whole-line-or-region windswap writeroom-mode yafolding yagist
                                    yaml-mode yard-mode yari)))

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:

(provide 'emacs)
;;; emacs.el ends here
