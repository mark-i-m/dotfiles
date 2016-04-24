#!/bin/bash -x

# A quick script to install pluggins

## To install or not to install?
##  - set to true if you want to install
##  - if you choose not to install something, you may need to go back and edit associated files

# Install all -- overrides the settings below
INSTALL_ALL=True

# bash
INSTALL_BASHRC=False
INSTALL_BASH_ENV_VARS=False
INSTALL_BASH_ALIASES=False

# xmonad
INSTALL_XMONAD_CONFIG=False
INSTALL_XMOBARRC=False

# vim
INSTALL_VIMRC=False

INSTALL_PATHOGEN=False

INSTALL_RUST_VIM=False
INSTALL_YCM=False
INSTALL_SYNTASTIC=False
INSTALL_TAGBAR=False
INSTALL_VIM_AIRLINE=False
INSTALL_VIM_FUGITIVE=False

# where to install
INSTALL_DIR=~

#################################################################################
## DO NOT EDIT ANYTHING BELOW THIS LINE!
#################################################################################

function install_config_file() {
    cp -r $1 $INSTALL_DIR
}

function install_vim_plugin() {
    mkdir -p $INSTALL_DIR/.vim/bundle
    ( cd ./vim/.vim/bundle && git submodule update --init --recursive $1 )
    cp -r ./vim/.vim/bundle/$1 $INSTALL_DIR/.vim/bundle/
}

# check to see if we should install everything
if [ "$INSTALL_ALL" = "True" ]; then
    INSTALL_BASHRC=True
    INSTALL_BASH_ENV_VARS=True
    INSTALL_BASH_ALIASES=True
    INSTALL_XMONAD_CONFIG=True
    INSTALL_XMOBARRC=True
    INSTALL_VIMRC=True
    INSTALL_PATHOGEN=True
    INSTALL_RUST_VIM=True
    INSTALL_YCM=True
    INSTALL_SYNTASTIC=True
    INSTALL_TAGBAR=True
    INSTALL_VIM_AIRLINE=True
    INSTALL_VIM_FUGITIVE=True
    INSTALL_VIM_SCALA=True
fi

# install bash stuff
if [ "$INSTALL_BASHRC" = "True" ]; then
    install_config_file "./bash/.bashrc"
fi

if [ "$INSTALL_BASH_ENV_VARS" = "True" ]; then
    install_config_file "./bash/.bash_env_vars"
fi

if [ "$INSTALL_BASH_ALIASES" = "True" ]; then
    install_config_file "./bash/.bash_aliases"
fi

# install xmonad stuff
if [ "$INSTALL_XMONAD_CONFIG" = "True" ]; then
    install_config_file "./wm/.xmonad"
fi

if [ "$INSTALL_XMOBARRC" = "True" ]; then
    install_config_file "./wm/.xmobarrc"
fi

# install vim
if [ "$INSTALL_VIMRC" = "True" ]; then
    install_config_file "./vim/.vimrc"
fi

if [ "$INSTALL_PATHOGEN" = "True" ]; then
    mkdir -p $INSTALL_DIR/.vim/autoload $INSTALL_DIR/.vim/bundle && \
        curl -LSso $INSTALL_DIR/.vim/autoload/pathogen.vim \
        https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi

if [ "$INSTALL_RUST_VIM" = "True" ]; then
    install_vim_plugin "rust.vim"
fi

if [ "$INSTALL_SYNTASTIC" = "True" ]; then
    install_vim_plugin "syntastic"
fi

if [ "$INSTALL_TAGBAR" = "True" ]; then
    install_vim_plugin "tagbar"
fi

if [ "$INSTALL_VIM_AIRLINE" = "True" ]; then
    install_vim_plugin "vim-airline"
fi

if [ "$INSTALL_VIM_FUGITIVE" = "True" ]; then
    install_vim_plugin "vim-fugitive"
fi

if [ "$INSTALL_YCM" = "True" ]; then
    install_vim_plugin "YouCompleteMe"
    ( cd $INSTALL_DIR/.vim/bundle/YouCompleteMe/ \
        && ./install.sh --clang-completer )
    cp ./vim/global_ycm_extra_conf.py $INSTALL_DIR/.vim/bundle/YouCompleteMe/
fi
