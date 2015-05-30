dotfilesvm
===========
This repo contains a stripped-down of the `dotfiles` repo intended for easy setup of VMs.

Config only
-----------
To install only the config files, follow the directions in `dotfiles`.

Full setup
----------
To do a full setup, run `./install_full.sh`. You will need to enter the password for `sudo`. This setup assumes you are using Ubuntu.

Altogether:
```
$ sudo apt-get install git
$ git clone https://github.com/mark-i-m/dotfilesvm.git && cd dotfilesvm
$ ./install_full.sh
```
