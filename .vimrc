" Special Thanks to Pato Lankenau
set nu                              " number lines
set splitright                      " when the split and split commands are used split to the right or below
set splitbelow
" set mouse=a                       " allow mouse navigation -- can get annoying with trackpadsset tabstop=4                       " size of a tab
set shiftwidth=4 
set expandtab                       " expand tabs into 4 spaces
syntax on
syntax enable                       " highlight syntax

" useful mappings for for tabs and split screens
map <F5> <ESC>:shell<CR>
map <F6> <ESC>:tabe<Space>
map <F7> <ESC>:split<Space>
map <F8> <ESC>:vsplit<Space>
map <C-Left> <ESC>:tabp<CR>
map <C-Right> <ESC>:tabn<CR>
map <S-Up> <C-W><Up>
map <S-Down> <C-W><Down>
map <S-Left> <C-W><Left>
map <S-Right> <C-W><Right>  

imap <F5> <ESC>:shell<CR>
imap <F6> <ESC>:tabe<Space>
imap <F7> <ESC>:split<Space>
imap <F8> <ESC>:vsplit<Space>
imap <C-Left> <ESC>:tabp<CR>
imap <C-Right> <ESC>:tabn<CR>
imap <S-Up> <ESC><C-W><Up>
imap <S-Down> <ESC><C-W><Down>
imap <S-Left> <ESC><C-W><Left>
imap <S-Right> <ESC><C-W><Right>

" useful aliases
cab W w
cab Q q 
cab latex Latex
cab Lat Latex 

ab teh the

" latex compilation
command Latex execute "silent !pdflatex % > compile.out &" | redraw!
command LatexDisplay execute "silent !pdflatex % > compile.out && xdg-open %:r.pdf > /dev/null 2>&1 &" | redraw!
command LatexBibtex execute "silent !pdflatex % > compile.out && bibtex %:r.aux >> compile.out && pdflatex % >> compile.out && pdflatex % >> compile.out &" | redraw!
command LatexBibtexDisplay execute "silent !pdflatex % > compile.out && bibtex %:r.aux >> compile.out && pdflatex % >> compile.out && pdflatex % >> compile.out && xdg-open %:r.pdf > /dev/null 2>&1 &" | redraw!

" arrow keys move display lines, not physical lines
noremap  <buffer> <silent> <Up>   gk
noremap  <buffer> <silent> <Down> gj
inoremap <buffer> <silent> <Up>   <C-o>gk
inoremap <buffer> <silent> <Down> <C-o>gj
