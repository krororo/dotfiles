#!/bin/bash

set -e

version="$1"

if [ -z $version ]; then
  echo "version not specified"
  exit 1
fi

function install_font() {
  local filename=$1
  local url_base="https://github.com/yuru7/HackGen/releases/download/${version}/"

  curl -sL -o /tmp/$filename "${url_base}${filename}"
  unzip -o /tmp/$filename -d /tmp
  cp /tmp/$(basename --suffix .zip $filename)/HackGen*.ttf $HOME/.fonts/
}

find $HOME/.fonts -name 'HackGen*.ttf' -delete

install_font HackGen_${version}.zip
install_font HackGen_NF_${version}.zip

fc-cache -fv

echo $version > $HOME/.fonts/.hackgen-vesion
