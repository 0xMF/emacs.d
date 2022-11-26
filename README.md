# A reasonable Emacs config for this vi/Vim user

Here's my setup for the [GNU Emacs](https://www.gnu.org/software/emacs/) editor. It builds on [Steve
Purcell's emacs.d](https://github.com/purcell/emacs.d) by making that configuration feel a bit more
natural to me - a seasoned vi/[Vim](https://www.vim.org) user.

In late 2017 I began by putting Emacs-specific settings into a sub-directory within my
[dotfiles](https://github.com/0xMF/dotfiles) repo but by May 2020 I found that strategy ineffective
and counter-intuitive when making significant commit history changes. Once after I cloned my
`dotfiles` on several minimal VMs I realized I would never use these GUI-based Emacs settings on
those VMs, that's when I decided to fork Purcell's repo and maintain my Emacs-specific settings
here thereby keeping both repos independent of each other at all times.

Purcell's `emacs.d` repo has proven to be dependable (logically consistent and remarkably stable)
over the years I used it. Besides the eye-candy settings Purcell keeps adding to his `emacs.d` has
made Emacs truly a delight for me to keep using. Thanks Steve! If you'd like to know more about
Purcell's setup, read his [README](README_Purcell.md) (archived here since July 2020).

# Warning
Unlike Purcell whose `emacs.d` repo is dependable, this repo has been at times the exact opposite.
I track upstream, apply changes on master, and may at times squash upstream (likely using `git push
-f`) either because I want a linear commit history or want the first settings that work. Gasp!

As of July 2022, I have started to use both Vim and Emacs daily, which means I know more Emacs and
Elisp than when I started (late 2017). My edits are in [init-0xMF-evil.el](lisp/init-0xMF-evil.el).
Since my knowledge of both Emacs and Elisp is limited, I tend to use the first available edit that
works, which means I cannot provide *any* support for *any* Emacs or Elisp feature configured here.

Experienced Emacs users may find my use of [Evil](https://github.com/emacs-evil/evil) mode and
[General](https://github.com/noctuid/general.el) in all their awesome glory appalling, so this repo
would not be their ideal choice.

In addition, experienced vi and Vim users may find my keybinding choice horribly confusing (as I
default to the Emacs keybindings whenever I see fit) so they, too, would benefit from alternative
approaches to using Emacs for vi/Vim users.

Should you find this repo beneficial, please consider [💝 supporting Purcell's Open Source
work](https://www.patreon.com/sanityinc).
