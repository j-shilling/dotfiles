#!/usr/bin/env bash

set -euo pipefail

function install_ohmyzsh {
    if hash curl 2>/dev/null ; then
        sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    elif hash wget 2>/dev/null  ; then
        sh -c "$(wget -O- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    elif hash fetch 2>/dev/null ; then
        sh -c "$(fetch -o - https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
    fi
}

function install_doom {
    if hash git 2>/dev/null ; then
        git clone --depth 1 https://github.com/hlissner/doom-emacs "${HOME}/.emacs.d"
    else
        echo >&2 "Can't install doom without git"
    fi
}

function install_doom_conf {
    if hash git 2>/dev/null ; then
        git clone --depth 1 git@gitlab.com:dotfiles11/doom.d.git "${HOME}/.doom.d"
        ${HOME}/.emacs.d/bin/doom sync
    else
        echo >&2 "Can't install doom config without git"
    fi
}

function install_tpm {
    if hash git 2>/dev/null ; then
        git clone https://github.com/tmux-plugins/tpm "${HOME}/.tmux/plugins/tpm"
    else
        echo >&2 "Can't install TPM without git"
    fi
}

function install_vim_plug {
    if hash curl 2>/dev/null ; then
	curl -fLo "${HOME}/.vim/autoload/plug.vim" --create-dirs \
	    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
    else
	echo >&2 "Can't install vim-plug without curl"
    fi
}

readonly script=$(basename ${0})
readonly dir=$(dirname $(realpath ${0}))
readonly files=$(git -C "${dir}" ls-files | grep -v "${script}")

[ -d "${HOME}/.oh-my-zsh/" ] || install_ohmyzsh
[ -d "${HOME}/.emacs.d/" ] || install_doom
[ -d "${HOME}/.doom.d/" ] || install_doom_conf
[ -d "${HOME}/.tmux/plugins/tpm" ] || install_tpm
[ -e "${HOME}/.vim/autoload/plug.vim" ] || install_vim_plug

for file in ${files} ; do
    name="$(basename ${file})"
    target="${HOME}/.${name}"

    echo -n "Checking for ${target}... "
    if [ -f "${target}" ] ; then
	echo "yes"
    else
	echo "no"
	ln -vs "${dir}/${file}" "${target}"
    fi
done
popd &>/dev/null
