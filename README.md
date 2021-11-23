# Usage
Clone this directory in home directory and call ./push.sh
---
git clone url
cd yotfiles
./push.sh
---

# TODO
Add markdown.vim and push to plugged/vim-markdown/syntax in push.sh
hmm seems to have some bug with overlapping wiht other syntax, investigate later
```
syn match mdlatex /$[^$]*\$/
hi mdlatex ctermfg=14  
```
