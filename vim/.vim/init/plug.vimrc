call plug#begin('~/.vim/plugged')
    " Theme
    Plug 'morhetz/gruvbox'

    " Fuzzy matching
    Plug 'junegunn/fzf.vim'
    Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }

    " Git Integration
    Plug 'tpope/vim-fugitive'
    Plug 'airblade/vim-gitgutter'

    " Async Commands
    Plug 'tpope/vim-dispatch'

    " Tmux
    Plug 'christoomey/vim-tmux-navigator'
    Plug 'preservim/vimux'
    Plug 'jpalardy/vim-slime'

    " General Programing
    Plug 'vim-syntastic/syntastic'
    Plug 'preservim/nerdcommenter'

    " LSP
    Plug 'prabirshrestha/vim-lsp'
    Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'mattn/vim-lsp-settings'

    " Clojure
    Plug 'tpope/vim-fireplace'
    Plug 'tpope/vim-salve'
    Plug 'kien/rainbow_parentheses.vim'
call plug#end()
