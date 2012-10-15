function InsertTabWrapper()
    let col = col('.') - 1
    if !col || getline('.')[col - 1] !~ '\k'
        return "\<tab>"
    else
        return "\<c-p>"
    endif
endfunction

imap <TAB> <C-r>=InsertTabWrapper()<CR>
set complete=.,k,b,t

set encoding=utf-8
set nocompatible

set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab

set ruler
set relativenumber
set mouse=a

set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch

nnoremap <up>    <nop>
nnoremap <down>  <nop>
nnoremap <left>  <nop>
nnoremap <right> <nop>
inoremap <up>    <nop>
inoremap <down>  <nop>
inoremap <left>  <nop>
inoremap <right> <nop>
nnoremap j gj
nnoremap k gk

set history=50

syntax on
colorscheme molokai
