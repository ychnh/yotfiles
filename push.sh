#!/bin/bash
cp .vimrc ~/
cp .tmux.conf ~/
cp .pylintrc ~/
mkdir ~/.config
mkdir ~/.config/nvim
cp init.vim ~/.config/nvim
pip3 install pynvim
