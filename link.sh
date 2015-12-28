#!/bin/bash -e

for f in .??*; do
    [[ "$f" == ".git" ]] && continue

    ln -sf dotfiles/$f ~/$f
done

