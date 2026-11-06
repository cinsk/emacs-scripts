#!/bin/bash

# curl -z
SOURCES=("https://raw.githubusercontent.com/fniessen/org-html-themes/refs/heads/master/org/setup/html-theme-readtheorg.setup"
	 "https://raw.githubusercontent.com/fniessen/org-html-themes/refs/heads/master/org/setup/html-theme-bigblow.setup")

for url in "${SOURCES[@]}"; do
    name=$(basename "$url")
    echo "# $name"
    curl -z "$name" -LOR "$url"
done

