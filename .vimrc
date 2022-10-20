"install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
      \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

"need manual install fzf and pluigvim

call plug#begin('~/.vim/plugged')

if has('nvim')
" Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  let g:jedi#completions_enabled = 0
else
" Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif
"let g:deoplete#enable_at_startup = 1

"tmux vim style switching
Plug 'christoomey/vim-tmux-navigator'
Plug 'https://github.com/henrik/vim-indexed-search'
" lint
Plug 'w0rp/ale'

Plug 'ncm2/ncm2'
Plug 'roxma/nvim-yarp'
Plug 'ychnh/vi_latex_preview'
Plug 'tpope/vim-surround'

Plug 'tmhedberg/SimpylFold'
Plug 'Konfekt/FastFold'

autocmd BufEnter * call ncm2#enable_for_buffer()

set completeopt=noinsert,menuone,noselect
" enable ncm2 for all buffers THIS IS NOT WORKING AT THE MOMENT
" IMPORTANT: :help Ncm2PopupOpen for more information
" NOTE: you need to install completion sources to get completions. https://github.com/ncm2/ncm2/wiki
"
Plug 'ncm2/ncm2-bufword'
Plug 'ncm2/ncm2-path'
Plug 'ncm2/ncm2-jedi'
Plug 'morhetz/gruvbox'
Plug 'andreasvc/vim-256noir'
Plug 'davidhalter/jedi-vim'
Plug 'itchyny/lightline.vim'
Plug '/usr/local/opt/fzf'
Plug 'scrooloose/nerdtree'
"Plug 'deoplete-plugins/deoplete-jedi'


Plug 'godlygeek/tabular'
"Plug 'plasticboy/vim-markdown'
call plug#end()
"
"Plugin Config -----------------------
"

let g:lightline = { 'colorscheme': 'PaperColor', }

" Linting
" let b:ale_linters = ['flake8', 'pylint']
let g:ale_linters = {'python':['pylint']}

let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_save = 1
let g:ale_lint_on_enter = 1
"let g:ale_rust_cargo_use_check = 1
"let g:ale_rust_cargo_check_all_targets = 1
" Syntax Highlighting ------------------

set nocursorline
set autoindent
set cindent
set number
set tabstop=2           " use 4 spaces to represent tab
set softtabstop=2
set shiftwidth=2
set ts=2 " Tab 너비
set shiftwidth=2 " 자동 인덴트할 때 너비
set expandtab


set laststatus=2 " 상태바 표시를 항상한다
set statusline=\ %<%l:%v\ [%P]%=%a\ %h%m%r\ %F\
set lazyredraw
set ttyfast
set showcmd
set backspace=indent,eol,start
set ruler
" Color Scheme
" colorscheme gruvbox
colorscheme 256_noir
set cursorline
highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212
autocmd InsertEnter * highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=234 guifg=NONE guibg=#1c1c1c
autocmd InsertLeave * highlight CursorLine cterm=NONE ctermfg=NONE ctermbg=233 guifg=NONE guibg=#121212

"set bg=dark
set t_Co=256
set ttimeoutlen=50
set noshowmode
set nowrap
syntax on
filetype plugin indent on

let g:jedi#use_splits_not_buffers = "bottom"
let g:jedi#show_call_signatures = "1"
let g:jedi#popup_select_first = "1"

"Hotkeys =======
" Move up/down by line
nnoremap j gj
nnoremap k gk

" Insert a new line without entering insert mode
"nmap oo o<Esc>
"nmap OO O<Esc>
nnoremap <F12> :buffers<CR>:buffer<Space>

"let g:SuperTabDefaultCompletionType = "<c-n>"
nnoremap <buffer> <F8> :exec '!python' shellescape(@%, 1)<cr>
nnoremap <c-s> :w<cr>
nnoremap <F11> :edit<Space>
nnoremap <F9> :bd<cr>
nnoremap > @q

nnoremap <F1> :call latex#Ltx()<cr>
" notebook compile hotkey
nnoremap <F2> :w<cr>:! cd ..;./build<cr>

nnoremap <m-s> <Esc>a<c-K>P*
inoremap <m-s> <c-K>P*

map L viWS$E

set hlsearch
set nofoldenable

:hi Search cterm=NONE ctermfg=grey ctermbg=yellow

let g:vim_markdown_math = 1

autocmd BufRead,BufNewFile {*.markdown,*.mdown,*.mkdn,*.md,*.mkd,*.mdwn,*.mdtxt,*.mdtext,*.text} set filetype=markdown
"autocmd FileType markdown setlocal syntax=off
autocmd BufNewFile,BufRead *.yhmd setf yhmd 
highlight yhmdlatex ctermfg=darkgray guifg=#00ffff
highlight yhmdstrike ctermfg=238 guifg=#ff0000
highlight yhmdem ctermfg=DarkRed guifg=#ff0000
highlight mathkeyword ctermfg=cyan guifg=#00ffff
highlight mathkeywordqqq ctermfg=cyan guifg=#00ffff


:hi Folded ctermbg=black
:hi Folded ctermfg=239
:setlocal foldmethod=expr foldexpr=getline(v:lnum)=~'^>!*'
set foldminlines=0



function MyFoldText()
  let nl = v:foldend - v:foldstart + 1
  let nucolwidth = &fdc + &number*&numberwidth
  let winwd = winwidth(0) - nucolwidth - 5
  let foldlinecount = foldclosedend(v:foldstart) - foldclosed(v:foldstart) + 1
  let prefix = " _______>>> "
  let fdnfo = prefix . string(v:foldlevel) . "," . string(foldlinecount)
  let line =  strpart(getline(v:foldstart), 0 , winwd - len(fdnfo))
  let fillcharcount = winwd - len(line) - len(fdnfo)
  "return line . " " . nl . repeat(" ",fillcharcount) . fdnfo
  return line . " " . nl . repeat(" ",fillcharcount)
endfunction
set foldtext=MyFoldText()

vnoremap <C-r> "hy:%s/<C-r>h//gc<left><left><left>

set breakindent
set breakindentopt=shift:2
set linebreak
