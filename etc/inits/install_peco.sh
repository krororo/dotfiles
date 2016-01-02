#!/bin/bash

url=$(
    curl -s https://api.github.com/repos/peco/peco/releases/latest |
        grep browser_download_url |
        grep linux_amd64 |
        cut -d '"' -f 4
   )
curl -sL "$url" |
    sudo tar xz -C /usr/local/bin \
         --strip=1 \
         --wildcards '*/peco' \
         --no-same-owner \
         --no-same-permissions
