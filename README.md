
Introduction
============

This is Seong-Kook Shin's personal GNU Emacs (primarily for version 24.x) scripts.

I use this scripts on both my Gentoo Linux, and my Macbook pro.

Here's a list of notable difference between other init scripts:

* Works on darwin based system (pre-selected fonts, etc.)
* no errors even if some packages are missing
* Korean support (mail, input-method, etc.)
* untabbify & remove-trailing spaces on various source code
* vc-jump (to jump VCS buffer on cvs/svn/git)
* mail address completion
* consistent key-bindings on interpreting lanauges (ruby/python/etc.)
* hungry-delete based on the syntactic context
* `desktop open` command on dired (works on X11 and darwin)
* scale up/down font size on mouse wheel
* pre-defined `org-html-export` (with bootstrap)
* better window/frame navigation
* `*uinit*` buffer reports the load time of custom scripts (for profiling)

Please, don't ask me of Windows configuration.  I hardly use it, so I can't help you.  However, I do accept any help or patches to support Windows.



List of used & customized packages 
==================================

* `package.el` support (even on Emacs version 23)
* ediff (for better frame handling, etc.)
* cc-mode (subword mode, linux kernel style, etc.)
* shell
* ibuffer
* hippie-expand
* iswitchb
* markdown
* vim-modeline
* magit
* lisp (emacs-lisp, common-lisp, slime, etc.)
* latex
* mmm-mode
* nxml (auto-correction on Gentoo installed schemas)
* dired-x
* xcscope
* mail
* yaml-mode
* css-mode
* cal-korea-x
* org-mode
* ruby-mode with inf-ruby
* python-mode
* maven
* w3m
* browse-url
* gnuplot
* go
* lua
* yasnippet
* scala2-mode with sbt-mode
* ess
* filladapt
* sunrise
* ido

Installation
============

Please back-up all your emacs scripts before installing this.

Download the scripts from github, and unpack in `$HOME/.emacs.d/`:

    $ cd
    $ git clone https://github.com/cinsk/emacs-scripts.git .emacs.d

That's all.
