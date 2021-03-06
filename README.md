
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
Emacs truly a delight to enjoy using. Thanks Steve! If you are new to Purcell's setup, read his
[README](README_Purcell.md) (archived here since July 2020).

# Warning

Unlike Purcell whose `emacs.d` repo is dependable, this repo may be the exact opposite because I
use `git push -f` to my publicly available repos whenever I want a linear git commit history
without needless commits. I often do all my work on master alone (Gasp!). This means I may squash,
remove, or amend commits and their contents at any time.

As this repo tracks upstream and applies all changes on top of upstream (to maintain a linear
commit history on this repo) meaning the side-effect of using `git push -f` *always* happens
whenever conflict changes are brought in from upstream.

My knowledge of both Emacs and Elisp is extremely limited so I simply use the first change that
works. I cannot provide *any* support for *any* Emacs or Elisp feature related to the setup here,
unfortunately.

Expert and experienced Emacs users may find my use [Evil](https://github.com/emacs-evil/evil)
mode and [General](https://github.com/noctuid/general.el) in all their glory appalling, so this
repo would not be their ideal choice. In addition, experienced vi and Vim users may find my
keybinding choice horribly confusing (as I default to the Emacs keybindings whenever I see fit) so
they, too, would benefit from alternative approaches to using Emacs for vi/Vim users.

# Support Purcell's work

Should you find this repo beneficial, please consider [💝 supporting Purcell's Open Source
work](https://www.patreon.com/sanityinc).

