# Additional bash commands(functions) interacts with Emacs
#
# To call Emacs command, the command should be registered in `ghostel-eval-cmds'.
# See ~/.emacs.d/init/_shell.el for more

dired() {
    [ $# -eq 0 ] && set -- "."
    ghostel_cmd dired-other-window "$@"
}

magit() {
    ghostel_cmd magit-status-setup-buffer "$(pwd)"
}

ff() {
    ghostel_cmd find-file-other-window "$@"
}

calc() {
    ghostel_cmd calc
}

ediff() {
    ghostel_cmd ediff-files "$@"
}

ediff3() {
    ghostel_cmd ediff-files3 "$@"
}

alias d=dired
