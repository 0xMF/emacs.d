# A reasonable Emacs config for this vi/Vim user

This is my setup for the [GNU Emacs](https://www.gnu.org/software/emacs/) editor.
It builds on [Steve Purcell's emacs.d](https://github.com/purcell/emacs.d) by making that configuration feel a bit more natural to me - a seasoned vi/[Vim](https://www.vim.org) user.

In late 2017 I put my Emacs-specific settings into a sub-directory of my [dotfiles](https://github.com/0xMF/dotfiles) repo.
By May 2020 I accumulated many commits that configured Emacs to work the way I liked to work with Vim.
Once after I cloned my `dotfiles` on several minimal Linux VMs I realized I would never use the GUI-based Emacs settings on those VMs.
That is when I forked Purcell's repo and started to track changes to upstream in this repo here.
The advantage of this approach means I can keep my dotfiles and emacs.d repos independent of each other.

Over the years of using of Purcell's `emacs.d` repo, I found it to be dependable (logically consistent and stable) yet seamlessly extensible to work with my own vi/Vim setup of [Evil mode](https://github.com/emacs-evil/evil) and [General](https://github.com/noctuid/general.el).
The eye-candy features Purcell keeps adding to his `emacs.d` has made Emacs truly a delight for me to keep using.
Thanks Steve!
If you are new to Purcell's setup, read his [README](README_Purcell.md) (archived here since July 2020).

# Warning

Unlike Purcell whose `emacs.d` repo is dependable, this repo can be and, at times, has been unstable because I value a linear commit history and will use the first Elisp or Emacs settings that work the way I want.
I track Purcell's upstream repo and apply my own settings to upstream master before merging it back here.
This means I squash upstream often using `git push -f`.
Gasp!
This means I may squash, remove, or amend commits and their contents at any time (and I often do so).

All my vi/Vim related setup is kept in [init-0xMF-evil.el](lisp/init-0xMF-evil.el).
As of July 2022, I use Emacs daily.
This means I know more Emacs and Elisp than when I started (late 2017).
So while my knowledge of Emacs and Elisp is more than when I began (late 2017) it is still limited.
I use the first edit that works the way I want Emacs to work like how I use Vim.

Experienced Emacs users may find my use of [Evil mode](https://github.com/emacs-evil/evil) and [General](https://github.com/noctuid/general.el) in all their awesome glory appalling.
Whereas experienced vi/Vim users may find some of my keybinding choices horribly confusing.
At times I default to sticking with the Emacs keybindings and Emacs way of doing things.

This repo tracks upstream and applies all my changes (using `git push -f`) on top of upstream (to maintain a linear commit history on this repo) meaning the side-effect of using `git push -f` *always* happens whenever conflict changes are brought in from upstream.

I cannot provide support for Emacs or Elisp that I configured here.

# Support Purcell's work

Should you find this repo beneficial, please consider [💝 supporting Purcell's Open Source work](https://www.patreon.com/sanityinc).
