#!/bin/bash

[ ! -d $HOME/src ] && mkdir $HOME/src

wget -O- http://ftp.gnu.org/gnu/emacs/emacs-24.5.tar.xz | tar xJf - -C $HOME/src
cd $HOME/src/emacs-24.5
./configure --disable-largefile \
  --without-toolkit-scroll-bars \
  --without-xaw3d \
  --without-xim \
  --without-sound \
  --without-pop \
  --without-xpm \
  --without-tiff \
  --without-rsvg \
  --without-gconf \
  --without-gsettings \
  --without-selinux \
  --without-gpm \
  --without-makeinfo \
  --with-x && make && sudo make install
