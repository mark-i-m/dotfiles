#!/bin/bash -x

# A quick script to install pluggins

## To install or not to install?
##  - set to true if you want to install
##  - if you choose not to install something, you may need to go back and edit associated files

# Install all -- overrides the settings below
INSTALL_ALL=False

# Xorg
INSTALL_PROFILE=False
INSTALL_XINITRC=False

# bash
INSTALL_BASHRC=False
INSTALL_BASH_ENV_VARS=False

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
INSTALL_VIM_SCALA=False

# where to install
INSTALL_DIR=~

#################################################################################
## DO NOT EDIT ANYTHING BELOW THIS LINE!
#################################################################################

# check to see if we should install everything
if [ "$INSTALL_ALL" = "True" ]; then
    INSTALL_PROFILE=True
    INSTALL_XINITRC=True
    INSTALL_BASHRC=True
    INSTALL_BASH_ENV_VARS=True
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

# install xorg stuff
if [ "$INSTALL_PROFILE" = "True" ]; then
    cp ./x/.profile $INSTALL_DIR/.profile
fi

if [ "$INSTALL_XINITRC" = "True" ]; then
    cp ./x/.xinitrc $INSTALL_DIR/.xinitrc
fi

# install bash stuff
if [ "$INSTALL_BASHRC" = "True" ]; then
    cp ./bash/.bashrc $INSTALL_DIR/.bashrc
fi

if [ "$INSTALL_BASH_ENV_VARS" = "True" ]; then
    cp ./bash/.bash_env_vars $INSTALL_DIR/.bash_env_vars
fi

# install xmonad stuff
if [ "$INSTALL_XMONAD_CONFIG" = "True" ]; then
    cp -r ./wm/.xmonad $INSTALL_DIR/.xmonad
fi

if [ "$INSTALL_XMOBARRC" = "True" ]; then
    cp ./wm/.xmobarrc $INSTALL_DIR/.xmobarrc
fi

# install vim
if [ "$INSTALL_VIMRC" = "True" ]; then
    cp ./vim/.vimrc $INSTALL_DIR/.vimrc
fi

if [ "$INSTALL_PATHOGEN" = "True" ]; then
    mkdir -p $INSTALL_DIR/.vim/autoload $INSTALL_DIR/.vim/bundle && \
        curl -LSso $INSTALL_DIR/.vim/autoload/pathogen.vim \
        https://raw.githubusercontent.com/tpope/vim-pathogen/master/autoload/pathogen.vim
fi

function install_vim_plugin() {
    mkdir -p $INSTALL_DIR/.vim/bundle
    ( cd ./vim/.vim/bundle && git submodule update --init $1 )
    cp -r ./vim/.vim/bundle/$1 $INSTALL_DIR/.vim/bundle/
}

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

if [ "$INSTALL_VIM_SCALA" = "True" ]; then
    install_vim_plugin "vim-scala"
fi

if [ "$INSTALL_YCM" = "True" ]; then
    install_vim_plugin "YouCompleteMe"
    ( cd $INSTALL_DIR/.vim/bundle/YouCompleteMe/ && ./install.sh --clang-completer )
fi
