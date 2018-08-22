#!/bin/bash -e

cd $(dirname $(readlink -f $0))

for f in .??* *.conf; do
  [[ "$f" == ".git" ]] && continue

  if [[ "$f" == ".config" ]]; then
    for ff in $f/**/*; do
      ln -sf ~/dotfiles/$ff ~/$ff
    done
  else
    ln -snf dotfiles/$f ~/$f
  fi
done
