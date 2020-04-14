# install stuff
sudo apt-get install build-essential cmake python-dev vim-gnome xmonad xmobar slock trayer nodejs npm konsole

# install rust
curl -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
rustup component add rust-src

# install universal-ctags
mkdir -p .local
git clone https://github.com/universal-ctags/ctags.git && \
    cd ctags && \
    ./autogen.sh && \
    ./configure --prefix=$HOME/.local && \
    make

# install starship
cargo install starship zoxide

# install config files
bash install.sh
