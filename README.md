
Introduction
============

This is my personal GNU Emacs setting.  I intend to use this setting on a couple of machines including:

* WSL2 Archlinux GNU Emacs (wayland) version 31.1
* macOS GNU Emacs version 30.2

Several Emacs packages are configured on my taste, including my own features.
This means my settings may override default key binding of existing features or that of third party package(s).

You may want to read [README.org](./README.org) for the details.


Installation
============

Make sure that you have recent GNU Emacs at least 23 or above. (I
highly recommend to use 24.3 or above)

        $ cd
        $ # Make sure that you back up the old .emacs.d/
        $ rm -rf .emacs.d
        $ git clone https://github.com/cinsk/emacs-scripts.git .emacs.d

That's all.

There may be several warnings for the missing packages, you can install them
via `M-x list-package` or `M-x package-install`.

Here are some recommended packages:

		magit
		vertico
		orderless
		marginalia
		embark-consult
		rg
		consult-ghostel
		flyspell-correct


