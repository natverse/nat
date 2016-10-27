#!/bin/sh
set -e
# check to see if cmtk install folder is empty
if [ ! -d "$HOME/usr/local/bin" ]; then
  git clone --depth 50 https://github.com/jefferis/cmtk
  cd cmtk/core && mkdir build && cd build && cmake .. && make DESTDIR=$HOME/ all install
else
  echo 'Using cached $HOME/usr/local directory.';
fi