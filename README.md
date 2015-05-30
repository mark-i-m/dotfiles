config-files
============
Contains various Linux config files

Some are based on other's config files

Installing
----------
To install the config setup, edit the top of `install.sh` to choose which settings to install *AND* where to install them. *Backup your old config files first!* Then, run `./install.sh`. You may need to `chmod +x install.sh` first.

Dependencies
------------
My window manager settup has a lot of dependencies. You probably won't need all of them, unless you are also running on Arch Linux.
- xmonad, xmobar depend on ghc and the haskell runtime
- slock, trayer, amixer, nm-applet, and other utilities need to be installed
- you may need to edit some of the scripts here to use your own drivers and settings

My vim setup uses several pluggins, which have their own dependencies. See the pluggins' repositories for their own dependencies. Here are is a short summary:
- tagbar: requires Exuberant ctags
- vim-airline: requires airline fonts (you can use it without the fonts, but it will look weird)
- YCM: requires clang and has a compiled part
    - install cmake and python headers beforehand (see the `Valloric/YouCompleteMe` repo)
