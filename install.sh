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
INSTALL_VIMPLUG=False
INSTALL_COCCONFIG=False

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
    INSTALL_VIMPLUG=True
    INSTALL_COCCONFIG=True
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

if [ "$INSTALL_VIMPLUG" = "True" ]; then
    mkdir -p $INSTALL_DIR/.vim/autoload && \
        curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim && \
        vim +PlugInstall +qall
fi

if [ "$INSTALL_COCCONFIG" = "True" ]; then
    mkdir -p $INSTALL_DIR/.vim
    cp coc-settings.json $INSTALL_DIR/.vim/
    cp coc-settings.json $INSTALL_DIR/.config/nvim/
fi
