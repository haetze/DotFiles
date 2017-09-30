"Richard Stewing
"Clean up for the .vimrc


filetype plugin indent on     " required!

set number
set history=50		" keep 50 lines of command line history
set expandtab
set tabstop=2
set background=light
au BufNewFile,BufRead *.go set ft=go
au BufNewFile,BufRead *.dart set ft=dart
au BufNewFile,BufRead *.json set ft=javascript


set loadplugins
set runtimepath^=$HOME/.vim/
call pathogen#infect()

map  <C-l> :tabn<CR>
map  <C-h> :tabp<CR>
map  <C-n> :tabnew<CR>

map <C-i> <C-W><C-J>
map <C-o> <C-W><C-K>
map <C-p> <C-W><C-L>
map <C-u> <C-W><C-H>

