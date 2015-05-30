# install stuff
sudo apt-get install build-essential cmake python-dev vim exuberant-ctags xmonad xmobar slock

# airline fonts
git clone https://github.com/powerline/fonts.git
(cd ./fonts; ./install.sh)

# install config files
bash install.sh
