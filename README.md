# A reasonable Emacs config for this vi/Vim user

Here's my setup for the [GNU Emacs](https://www.gnu.org/software/emacs/) editor. It builds on [Steve
Purcell's emacs.d](https://github.com/purcell/emacs.d) by making that configuration feel a bit more
natural to me - a seasoned vi/[Vim](https://www.vim.org) user.

In late 2017 I began by putting Emacs-specific settings into a sub-directory within my
[dotfiles](https://github.com/0xMF/dotfiles) repo but by May 2020 I found that strategy ineffective
and counter-intuitive when making significant commit history changes. Once after I cloned my
`dotfiles` on several minimal VMs I realized I would never use these GUI-based Emacs settings on
those VMs. That is when I decided to fork Purcell's repo and maintain my Emacs-specific settings
here. Thereby keeping both my dotfiles and emacs.d repos independent of each other at all times.

Over the years of using of Purcell's `emacs.d` repo, I found it to be dependable (logically
consistent and remarkably stable) yet seamlessly extensible to my vi/Vim keybinding setup of [Evil
mode](https://github.com/emacs-evil/evil). The eye-candy features Purcell keeps adding to his
`emacs.d` has made Emacs truly a delight for me to keep using. Thanks Steve! If you'd like to know
more about Purcell's setup, read his [README](README_Purcell.md) (archived here since July 2020).

# Warning
Unlike Purcell whose `emacs.d` repo is dependable, this repo can be and has been at times unstable.
I value a linear commit history and use the first settings that work. I track Purcell's upstream
repo and apply my changes to master. This means I squash upstream often using `git push -f`. Gasp!

All my setup is in [init-0xMF-evil.el](lisp/init-0xMF-evil.el). As of July 2022 I use Vim and Emacs
daily. So my knowledge of Emacs and Elisp is a bit more than when I started (late 2017). Despite
that my knowledge of Emacs and Elisp is limited and because I use the first edit that works, means
I cannot provide *any* support for *any* Emacs or Elisp feature that I configured here.

Experienced Emacs users may find my use of [Evil mode](https://github.com/emacs-evil/evil) and
[General](https://github.com/noctuid/general.el) in all their awesome glory appalling. In addition,
experienced vi and Vim users may find my keybinding choice horribly confusing because at times I
default to Emacs keybindings when I see fit.

# Support Purcell's work

Should you find this repo beneficial, please consider [💝 supporting Purcell's Open Source
work](https://www.patreon.com/sanityinc).
