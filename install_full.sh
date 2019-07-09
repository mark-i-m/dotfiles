# install stuff
sudo apt-get install build-essential cmake python-dev vim-gnome exuberant-ctags xmonad xmobar slock trayer

# install rust
curl -sSf https://sh.rustup.rs | sh -s -- -y --no-modify-path
rustup component add rust-src

# install rust-analyzer
git clone https://github.com/rust-analyzer/rust-analyzer && cd rust-analyzer && cargo install-lsp

# install universal-ctags
mkdir -p .local
git clone https://github.com/universal-ctags/ctags.git && \
    cd ctags && \
    ./autogen.sh && \
    ./configure --prefix=$HOME/.local && \
    make

# install config files
bash install.sh
