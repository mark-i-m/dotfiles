# install stuff
sudo apt-get install build-essential cmake python-dev vim-gnome exuberant-ctags xmonad xmobar slock trayer

# install rust
curl -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path

# install config files
bash install.sh
