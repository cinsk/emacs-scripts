#!/bin/bash

# See http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/


uname -a | grep -i microsoft &>/dev/null
if [ $? -eq 0 ]; then
    WSL=1
fi

if [ -z "$WSL" ]; then
    for f in "$@"; do
        nohup xdg-open "$f" &
    done
else
    for f in "$@"; do
        nohup cmd.exe /c start $(wslpath -w "$f") &
    done
fi

# why 1 second? perhaps 0.5 would be better? -- cinsk
sleep 1

