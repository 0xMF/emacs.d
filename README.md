# A reasonable Emacs config for a seasoned vi/Vim user

This is my setup for the [GNU Emacs](https://www.gnu.org/software/emacs/) editor. It tweaks [Steve
Purcell's emacs.d](https://github.com/purcell/emacs.d) and makes his setup more natural for me, a
seasoned vi/[Vim](https://www.vim.org) user, to use.

In late 2017 I began by putting Emacs-specific settings into a sub-directory within my
[dotfiles](https://github.com/0xMF/dotfiles) repo but by May 2020 I found that strategy ineffective
and counter-intuitive particularly when making significant commit history changes. Once after I
cloned my `dotfiles` on several machines and realized I probably would never use the desktop
version of Emacs on those machines, I decided to fork Purcell's repo and put all my Emacs settings
in here so these settings and my `dotfiles` settings continue to be independent of each other at
all times.

Purcell's `emacs.d` repo proved dependable (logically consistent and remarkably stable) over
the years I used it. The eye-candy features Purcell keeps adding to his `emacs.d` setup has made
Emacs truly a delight that I enjoy using. Thanks Steve! If you are new to Purcell's setup, read his
[README](README_Purcell.md) (archived here since July 2020).

# Warning

Unlike Purcell whose `emacs.d` repo is dependable, this repo can be and, at times, has been unstable because I value a linear commit history and will use the first Elisp or Emacs settings that work the way I want.
I track Purcell's upstream repo and apply my own settings to upstream master before merging it back here.
This means I squash upstream often using `git push -f`.
Gasp!
This means I may squash, remove, or amend commits and their contents at any time (and I often do so).

As of July 2022, I use Emacs daily, which means I know a bit more Emacs and Elisp than when I started.
All my vi/Vim setup is in [init-0xMF-evil.el](lisp/init-0xMF-evil.el) but my knowledge of both Emacs and Elisp is limited.
I use the first change that works. Unfortunately, I cannot provide *any* support for *any* Emacs or Elisp feature related to this setup.

Experienced Emacs users may find my use of [Evil mode](https://github.com/emacs-evil/evil) and [General](https://github.com/noctuid/general.el) in all their awesome glory appalling.
Whereas experienced vi/Vim users may find some of my keybinding choices horribly confusing.
At times I default to sticking with the Emacs keybindings and Emacs way of doing things.

This repo tracks upstream and applies all my changes (using `git push -f`) on top of upstream (to maintain a linear commit history on this repo) meaning the side-effect of using `git push -f` *always* happens whenever conflict changes are brought in from upstream.

I cannot provide support for Emacs or Elisp that I configured here.

# Support Purcell's work

Should you find this repo beneficial, please consider [💝 supporting Purcell's Open Source work](https://www.patreon.com/sanityinc).
