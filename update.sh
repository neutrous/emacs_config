#! /usr/bin/env bash

if [ ! -d ./.emacs.d ]; then
	mkdir .emacs.d
fi

# copy the files into this directory
cp -f $HOME/.emacs.d/*.el ./.emacs.d/

# copy the snippets
if [ -e $HOME/.emacs.d/snippets ]; then
	cp -Ruf $HOME/.emacs.d/snippets/ ./.emacs.d/
	git add ./.emacs.d/snippets
fi

# copy the extra info files
if [ -e $HOME/.emacs.d/info ]; then
	cp -Ruf $HOME/.emacs.d/info/ ./.emacs.d/
	git add -A ./.emacs.d/info
fi
	
cp $HOME/.emacs ./

# git add .emacs
# git add ./.emacs.d/*.el
# git add README.md
# git add update.sh

# git commit -m "$1"
