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
pip3 install pynvim
