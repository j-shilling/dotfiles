let mapleader=" "

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
    " Theme
    Plug 'morhetz/gruvbox'

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
<<<<<<< HEAD
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'mattn/vim-lsp-settings'
=======
    Plug 'mattn/vim-lsp-settings'
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
>>>>>>> a0a711ff0075c5a23c4ff4c49175de065527dcee

    " Clojure
    Plug 'tpope/vim-fireplace'
    Plug 'tpope/vim-salve'
    Plug 'kien/rainbow_parentheses.vim'
call plug#end()

" Activate gruvbox theme
autocmd vimenter * ++nested colorscheme gruvbox
set background=dark

" Use Rg instead of Grep
set grepprg=rg\ --vimgrep\ --smart-case\ --follow

<<<<<<< HEAD
nnoremap <C-x><C-f> :Files<CR>
nnoremap <C-x>b :Buffers<CR>
nnoremap <leader>ss :BLines<CR>
nnoremap <leader>pf :GFiles<CR>
nnoremap <leader>sp :Rg<CR>

if executable('ccls')
    au User lsp_setup call lsp#register_server({
		\ 'name': 'ccls',
		\ 'cmd': {server_info->['ccls']},
		\ 'root_uri': {server_info->lsp#utils#path_to_uri(
		\   lsp#utils#find_nearest_parent_file_directory(
		\     lsp#utils#get_buffer_path(), ['.ccls', 'compile_commands.json', '.git/']))},
		\ 'initialization_options': {
		\   'highlight': { 'lsRanges' : v:true },
		\ },
		\ 'whitelist': ['c', 'cpp', 'objc', 'objcpp', 'cc'],
		\ })
endif
=======
" FZF
nnoremap <C-x>f :Files<CR>
nnoremap <C-x>b :Buffers<CR>
nnoremap <leader>pf :GFiles<CR>
nnoremap <leader>ss :Lines<CR>

" LSP
>>>>>>> a0a711ff0075c5a23c4ff4c49175de065527dcee

function! s:on_lsp_buffer_enabled() abort
    setlocal omnifunc=lsp#complete
    setlocal signcolumn=yes
<<<<<<< HEAD
    if exists('+tagfunc') | setlocal tagfunc=lsp#tagfunc | endif
    nmap <buffer> <leader>cd <plug>(lsp-definition)
    nmap <buffer> <leader>cD <plug>(lsp-references)
    nmap <buffer> <leader>cr <plug>(lsp-rename)

    let g:lsp_format_sync_timeout = 1000
    autocmd! BufWritePre *.rs,*.go call execute('LspDocumentFormatSync')
endfunction
=======
    nmap <buffer> <leader>cd <plug>(lsp-definition)
    nmap <buffer> <leader>cD <plug>(lsp-references)
endfunction

augroup lsp_install
    au!
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" Slime
>>>>>>> a0a711ff0075c5a23c4ff4c49175de065527dcee

augroup lsp_install
    au!
    " call s:on_lsp_buffer_enabled only for languages that has the server registered.
    autocmd User lsp_buffer_enabled call s:on_lsp_buffer_enabled()
augroup END

" Slime
let g:slime_target = "tmux"
let g:slime_default_config = {"socket_name": "default", "target_pane":"{last}"}

