#!/bin/bash -x

# A quick script to install pluggins

## To install or not to install?
##  - set to true if you want to install
##  - if you choose not to install something, you may need to go back and edit associated files

# Install all -- overrides the settings below
INSTALL_ALL=True

# bash
INSTALL_BASHRC=False

# xmonad
INSTALL_XMONAD_CONFIG=False
INSTALL_XMOBARRC=False

# vim
INSTALL_VIMRC=False
INSTALL_VUNDLE=False
INSTALL_YCM=False

# where to install
INSTALL_DIR=~

#################################################################################
## DO NOT EDIT ANYTHING BELOW THIS LINE!
#################################################################################

function install_config_file() {
    cp -r $1 $INSTALL_DIR
}

# check to see if we should install everything
if [ "$INSTALL_ALL" = "True" ]; then
    INSTALL_BASHRC=True
    INSTALL_XMONAD_CONFIG=True
    INSTALL_XMOBARRC=True
    INSTALL_VIMRC=True
    INSTALL_VUNDLE=True
    INSTALL_YCM=True
fi

# install bash stuff
if [ "$INSTALL_BASHRC" = "True" ]; then
    install_config_file "./bash/.bashrc"
    install_config_file "./bash/.git_prompt.sh"
    install_config_file "./bash/.bash_env_vars"
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

if [ "$INSTALL_VUNDLE" = "True" ]; then
    mkdir -p $INSTALL_DIR/.vim/bundle && \
        git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim && \
        vim +PluginInstall +qall
fi
