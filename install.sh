#!/bin/bash -e

git clone https://github.com/krororo/dotfiles.git ~/dotfiles
(cd ~/dotfiles; ./link.sh)
