#!/bin/bash
cp .vimrc ~/
cp .tmux.conf ~/
cp .pylintrc ~/
mkdir ~/.config
mkdir ~/.config/nvim
cp init.vim ~/.config/nvim
mkdir ~/.vim
mkdir ~/.vim/syntax
cp markdown.vim ~/.vim/syntax
mkdir ~/.emacs.d
cp .emacs.d/init.el ~/.emacs.d/init.el
pip3 install pynvim
