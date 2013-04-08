#!/bin/bash

# See http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/

for f in "$@"; do
    nohup xdg-open "$f" &
done

# why 1 second? perhaps 0.5 would be better? -- cinsk
sleep 1

