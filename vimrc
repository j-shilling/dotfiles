set number relativenumber
set nowrap
set showmatch
set visualbell

set hlsearch
set smartcase
set ignorecase
set incsearch

set autoindent
set shiftwidth=4
set smartindent
set smarttab
set softtabstop=4

set ruler
set backspace=indent,eol,start
set autochdir

set encoding=utf-8
set hidden
set nobackup
set nowritebackup

set cmdheight=2
set updatetime=300
set shortmess+=c

if has("patch-8.1.1564")
    " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

call plug#begin('~/.vim/plugged')
    " Fuzzy matching
    Plug 'junegunn/fzf.vim'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

    " Git Integration
    Plug 'tpope/vim-fugitive'

    " LSP
    Plug 'neoclide/coc.nvim', {'branch': 'release'}
call plug#end()

" Use Rg instead of Grep
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

