let mapleader = ' '

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

" Split Navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

set splitbelow
set splitright

if has("patch-8.1.1564")
    " Recently vim can merge signcolumn and number column into one
    set signcolumn=number
else
    set signcolumn=yes
endif

" Powerline
python3 from powerline.vim import setup as powerline_setup
python3 powerline_setup()
python3 del powerline_setup
set laststatus=2

call plug#begin('~/.vim/plugged')
    " Fuzzy matching
    Plug 'junegunn/fzf.vim'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

    " Git Integration
    Plug 'tpope/vim-fugitive'

    " Tmux
    Plug 'christoomey/vim-tmux-navigator'
    Plug 'preservim/vimux'
    Plug 'jpalardy/vim-slime'

    " LSP
    Plug 'prabirshrestha/vim-lsp'
    Plug 'mattn/vim-lsp-settings'
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'

    " Clojure
    Plug 'tpope/vim-fireplace'
    Plug 'tpope/vim-salve'
    Plug 'kien/rainbow_parentheses.vim'
call plug#end()

" Use Rg instead of Grep
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

" FZF
nnoremap <C-x>f :Files<CR>
nnoremap <C-x>b :Buffers<CR>
nnoremap <leader>pf :GFiles<CR>
nnoremap <leader>ss :Lines<CR>

" LSP

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
    nmap <buffer> <leader>cd <plug>(lsp-definition)
    nmap <buffer> <leader>cD <plug>(lsp-references)
endfunction

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" Slime

let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane":"{last}"}

