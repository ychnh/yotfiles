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

```
scripts
xrandr --output HDMI-1-0 --auto --same-as eDP --mode 2560x1440
./xflux -z 90011 -k 2000
```

# TODO
Add markdown.vim and push to plugged/vim-markdown/syntax in push.sh
hmm seems to have some bug with overlapping wiht other syntax, investigate later
```
syn match mdlatex /$[^$]*\$/
hi mdlatex ctermfg=14  
```
