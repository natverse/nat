#!/bin/sh
set -e
# check to see if cmtk install folder is empty
if [ ! -d "$HOME/usr/local/bin" ]; then
  mkdir -p $HOME/src && cd $HOME/src
  git clone --depth 50 https://github.com/jefferis/cmtk@natdev
  cd cmtk/core && mkdir build && cd build && cmake .. && make DESTDIR=$HOME/ all install
else
  echo 'Using cached $HOME/usr/local directory.';
fi
