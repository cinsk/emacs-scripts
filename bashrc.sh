#!/bin/sh
# This file is automatically read by Emacs on `M-x shell'.

if test "x$INSIDE_EMACS" != "x"; then
    if test "x$TERM" == "xdumb"; then
        # Issuing GIT command in shell mode (`M-x shell') is
        # distracting since it uses less(1) as a pager.  Setting
        # GIT_PAGER and MANPAGER will prevent to use less(1) in shell
        # mode.
        export GIT_PAGER="/bin/cat"
        export MANPAGER="/bin/cat"
    fi
fi