# install stuff
sudo apt-get install build-essential cmake python-dev vim-gnome exuberant-ctags xmonad xmobar slock trayer

# airline fonts
git clone https://github.com/powerline/fonts.git
(cd ./fonts; ./install.sh)

# install rust
curl -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path

# install config files
bash install.sh
