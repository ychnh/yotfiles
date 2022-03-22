#!/bin/bash
cp .vimrc ~/
cp .tmux.conf ~/
cp .pylintrc ~/
mkdir ~/.config
mkdir ~/.config/nvim
cp init.vim ~/.config/nvim
mkdir ~/.vim
mkdir ~/.vim/syntax
cp yhmd.vim ~/.vim/syntax
pip3 install pynvim
