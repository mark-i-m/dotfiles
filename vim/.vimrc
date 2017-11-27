
set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

Plugin 'scrooloose/nerdtree'

Plugin 'rust-lang/rust.vim'

Plugin 'vim-syntastic/syntastic'

Plugin 'majutsushi/tagbar'

Plugin 'vim-airline/vim-airline'

Plugin 'Valloric/YouCompleteMe'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

" vim-airline
set timeoutlen=50
set laststatus=2
set t_Co=256

let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" tagbar
" let g:tagbar_show_linenumbers=1

" autocomplete
" YCM
let g:ycm_server_python_interpreter = 'python2'
" let g:clang_complete_auto = 1
" let g:clang_use_library = 1
" let g:clang_debug = 1
" let g:clang_library_path = '/usr/lib/'
" let g:clang_user_options='|| exit 0'
let g:ycm_global_ycm_extra_conf = $HOME . '/.vim/bundle/global_ycm_extra_conf.py'
" set noshowmode " hide annoying User Defined Completion msg
set completeopt-=preview " hide annoying preview window

" rust autoformatting
let g:rustfmt_autosave = 1

" normal config stuff follows:

set nu                              " number lines
set splitright                      " when the split and split commands are used split to the right or below
set splitbelow
" set mouse=a                       " allow mouse navigation -- can get annoying with trackpads
set tabstop=4                       " size of a tab
set shiftwidth=4
set expandtab                       " expand tabs into 4 spaces
syntax on
syntax enable                       " highlight syntax
set tabpagemax=10000
set nowrap

" useful mappings for for tabs and split screens
map <F4> <ESC>:YcmCompleter GoTo<CR>
if has("nvim")
  map <F5> <ESC>:tabe\|term<CR>
else
  map <F5> <ESC>:shell<CR>
endif
map <F6> <ESC>:tabe<Space>
map <F7> <ESC>:split<Space>
map <F8> <ESC>:vsplit<Space>
map <C-Left> <ESC>:tabp<CR>
map <C-Right> <ESC>:tabn<CR>
map <S-Up> <C-W><Up>
map <S-Down> <C-W><Down>
map <S-Left> <C-W><Left>
map <S-Right> <C-W><Right>
map <F9> <ESC>:Tagbar<CR>
map <C-p> <ESC>"+p
map <C-y> "+y
map <C-Up> 10<Up>
map <C-Down> 10<Down>
map <F10> <ESC>:NERDTreeToggle<CR>

imap <F4> <ESC>:YcmCompleter GoTo<CR>
if has("nvim")
  imap <F5> <ESC>:tabe\|term<CR>
else
  imap <F5> <ESC>:shell<CR>
endif
imap <F6> <ESC>:tabe<Space>
imap <F7> <ESC>:split<Space>
imap <F8> <ESC>:vsplit<Space>
imap <C-Left> <ESC>:tabp<CR>
imap <C-Right> <ESC>:tabn<CR>
imap <S-Up> <ESC><C-W><Up>
imap <S-Down> <ESC><C-W><Down>
imap <S-Left> <ESC><C-W><Left>
imap <S-Right> <ESC><C-W><Right>
imap <F9> <ESC>:Tagbar<CR>
imap <C-p> <ESC>"+p
imap <C-y> "+y
imap <C-Up> 10<Up>
imap <C-Down> 10<Down>
imap <F10> <ESC>:NERDTreeToggle<CR>

if has("nvim")
  tnoremap <ESC> <C-\><C-n>
  tnoremap <F5> <C-\><C-n>:tabe\|term<CR>
  tnoremap <F6> <C-\><C-n>:tabe<Space>
  tnoremap <F7> <C-\><C-n>:split<Space>
  tnoremap <F8> <C-\><C-n>:vsplit<Space>
  tnoremap <C-Left> <C-\><C-n>:tabp<CR>
  tnoremap <C-Right> <C-\><C-n>:tabn<CR>
  tnoremap <C-p> <C-\><C-n>"+p<ESC>i
  tnoremap <C-y> "+y
endif

" arrow keys move display lines, not physical lines
noremap  <buffer> <silent> <Up>   gk
noremap  <buffer> <silent> <Down> gj
inoremap <buffer> <silent> <Up>   <C-o>gk
inoremap <buffer> <silent> <Down> <C-o>gj

" useful aliases
cab W w
cab Q q
cab X x
cab Wa wa
cab Qa qa
cab Xa xa
cab latex Latex
cab LAtex Latex
cab Lat Latex
cab LAt Latex
cab lat Latex

ab teh the

" remove trailing whitespace
command RemoveTrailingWhitespace %s/\s\+$//

" Change colorscheme ... should always be at the end
colorscheme desert

set cursorline
highlight CursorLine cterm=none ctermbg=233

highlight Pmenu ctermbg=blue ctermfg=white guibg=blue guifg=white
highlight SpellBad ctermbg=red ctermfg=yellow guibg=red guifg=yellow
highlight Search ctermbg=darkblue ctermfg=yellow cterm=bold
highlight VertSplit ctermbg=black ctermfg=black

" latex compilation
command LatexClean execute "silent !rm -f /tmp/%:r.log /tmp/%:r.aux %:r.pdf" | redraw!
command LatexDisplay execute "silent !xdg-open %:r.pdf > /dev/null 2>&1 &" | redraw!
command LatexBibtex execute "silent !bibtex /tmp/%:r.aux >> /tmp/%.compile.out" | redraw!

function LatexCompile()
    silent !pdflatex -interaction=nonstopmode -output-directory /tmp/ % > "/tmp/%.compile.out"

    " Also do Bibtex compile if there is a .bib file available
    if !empty(glob("*.bib"))
        silent !bibtex /tmp/%:r.aux >> /tmp/%.compile.out
        silent !pdflatex -interaction=nonstopmode -output-directory /tmp/ % > "/tmp/%.compile.out"
        silent !pdflatex -interaction=nonstopmode -output-directory /tmp/ % > "/tmp/%.compile.out"
    endif

    if filereadable('/tmp/' . expand('%:r') . ".pdf")
        " If PDF was generated, take it
        silent !mv /tmp/%:r.pdf .
    else
        " Otherwise, report error
        echo "PDF not produced :("
        echo "Please see /tmp/" . expand('%') . ".compile.out for errors."
    endif
endfunction

function LatexClean()
    " remove artifacts
    silent !rm -f /tmp/%:r.log /tmp/%:r.aux %:r.pdf
endfunction

function LatexDisplay()
    " Check if there is a PDF
    if !filereadable(expand('%:r') . ".pdf")
        call LatexCompile()
    endif

    " Check if it worked
    if filereadable(expand('%:r') . ".pdf")
        silent !xdg-open %:r.pdf > /dev/null 2>&1 &
    else
        echoerr "PDF not produced :("
    endif
endfunction

" Automatically compile on write
autocmd BufReadPost *.tex call LatexClean()
autocmd BufWritePost *.tex call LatexCompile()
