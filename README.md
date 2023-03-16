# Usage
Clone this directory in home directory and call ./push.sh
---
git clone url
cd yotfiles
./push.sh
---

# Future
* install vimplug
* sudo apt-get install python3-neovim
* sudo apt-get install python3-pip
* pip3 install jedi
* install flux
* run scripts on startup

* dwm https://dwm.suckless.org/customisation/windows_key/

```
scripts just add in for now # .xinputrc
# im-config(8) generated on Wed, 15 Mar 2023 16:26:05 +0900
run_im fcitx
# im-config signature: a7673d91aeb6a4da7061b047288d2e59  -
#
xrandr --output eDP-1 --mode 1680x1050 --set 'scaling mode' Full
xrandr --output eDP --mode 1680x1050 --set 'scaling mode' Full
xrandr --output HDMI-1-0 --auto --same-as eDP-1 --mode 2560x1440
xrandr --output HDMI-1-0 --auto --same-as eDP --mode 2560x1440

./xflux -z 90011 -k 2500
sleep 0.5
xmodmap -e 'clear Lock' -e 'keycode 0x42 = Super_L
```

# TODO
Add markdown.vim and push to plugged/vim-markdown/syntax in push.sh
hmm seems to have some bug with overlapping wiht other syntax, investigate later
```
syn match mdlatex /$[^$]*\$/
hi mdlatex ctermfg=14  
```
