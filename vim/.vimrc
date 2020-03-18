
set nocompatible              " vim, no vi

call plug#begin('~/.vim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'rust-lang/rust.vim'
Plug 'rust-analyzer/rust-analyzer', { 'do': 'cargo install-ra --server' }
Plug 'majutsushi/tagbar'
Plug 'vim-airline/vim-airline'
Plug 'neoclide/coc.nvim', { 'branch': 'release' }
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install' }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/neovim-remote', { 'dir': '~/.nvr', 'do': 'pip3 install --user -e .' }
call plug#end()

" vim-airline
set timeoutlen=50
set laststatus=2
set t_Co=256

let g:airline_powerline_fonts = 0
let g:airline#extensions#tabline#enabled = 0

" coc.vim
set hidden
set updatetime=300
set shortmess+=c
" autocomplete
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" show docs
nnoremap <silent> K :call <SID>show_documentation()<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction
" highlight uses on hover
autocmd CursorHold * silent call CocActionAsync('highlight')
" Go to prev diagnostic Alt-w
nmap <silent> <M-w> <Plug>(coc-diagnostic-prev)
" Go to prev error diagnostic Alt-w
nmap <silent> <M-e> <Plug>(coc-diagnostic-prev-error)
" Go to references Alt-r
nmap <silent> <M-r> <Plug>(coc-references)
" Go to def Alt-d
nmap <silent> <M-d> <Plug>(coc-definition)
" Rename with Alt-f
nmap <silent> <M-f> <Plug>(coc-rename)


" fzf
" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1
let g:fzf_action = { 'enter': 'tab split' }

" fzf + Rg with preview window
command! -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1,
  \   fzf#vim#with_preview('right:50%', '?'))

" fzf word under cursor
nmap <silent> S :call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(expand('<cword>')), 1,
  \   fzf#vim#with_preview('right:50%', '?'))<CR>

" fzf selected text
vmap <silent> S "yy:call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(@y), 1,
  \   fzf#vim#with_preview('right:50%', '?'))<CR>

" rust autoformatting
let g:rustfmt_autosave = 1

" use universal-ctags for Rust tagbar
let g:rust_use_custom_ctags_defs = 1
let g:tagbar_type_rust = {
  \ 'ctagsbin' : '/home/mark/.local/bin/ctags',
  \ 'ctagstype' : 'rust',
  \ 'kinds' : [
      \ 'n:modules',
      \ 's:structures:1',
      \ 'i:interfaces',
      \ 'c:implementations',
      \ 'f:functions:1',
      \ 'g:enumerations:1',
      \ 't:type aliases:1:0',
      \ 'v:constants:1:0',
      \ 'M:macros:1',
      \ 'm:fields:1:0',
      \ 'e:enum variants:1:0',
      \ 'P:methods:1',
  \ ],
  \ 'sro': '::',
  \ 'kind2scope' : {
      \ 'n': 'module',
      \ 's': 'struct',
      \ 'i': 'interface',
      \ 'c': 'implementation',
      \ 'f': 'function',
      \ 'g': 'enum',
      \ 't': 'typedef',
      \ 'v': 'variable',
      \ 'M': 'macro',
      \ 'm': 'field',
      \ 'e': 'enumerator',
      \ 'P': 'method',
  \ },
\ }

" nvr
if has('nvim')
  let $GIT_EDITOR = 'nvr -cc tabe --remote-wait'
endif
autocmd FileType gitcommit,gitrebase,gitconfig set bufhidden=delete

" normal config stuff follows:

set nu                              " number lines
set splitright                      " when the split and split commands are used split to the right or below
set splitbelow
" set mouse=a                         " allow mouse navigation -- can get annoying with trackpads
set tabstop=4                       " size of a tab
set shiftwidth=4
set expandtab                       " expand tabs into 4 spaces
syntax on
syntax enable                       " highlight syntax
set tabpagemax=10000
set nowrap
set ai                              " autoindent
set incsearch                        " jump to incremental search results
set hlsearch                        " highlight search results
set scrolloff=1                     " always show at least one line above and below cursor
set wrap                            " wrap long lines
set breakindent                     " when wrapping, indent at the break
set breakindentopt=sbr              " show the break...
set showbreak=╰>\                   " ... with these characters
set wildmode=longest:full           " tab-completion of vim commands in normal mode
set wildmenu

" Use <C-L> to clear the highlighting of :set hlsearch.
if maparg('<C-L>', 'n') ==# ''
  nnoremap <silent> <C-L> :nohlsearch<C-R>=has('diff')?'<Bar>diffupdate':''<CR><CR><C-L>
endif

function! Tabline()
  let s = ''
  for i in range(tabpagenr('$'))
    let tab = i + 1
    let winnr = tabpagewinnr(tab)
    let buflist = tabpagebuflist(tab)
    let bufnr = buflist[winnr - 1]
    let bufname = bufname(bufnr)
    let bufmodified = getbufvar(bufnr, "&mod")

    let s .= '%' . tab . 'T'
    let s .= (tab == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' '
    let s .= (bufname != '' ? ''. fnamemodify(bufname, ':t') . ' ' : '[No Name] ')

    if bufmodified
      let s .= '[+] '
    endif
  endfor

  let s .= '%#TabLineFill#'
  if (exists("g:tablineclosebutton"))
    let s .= '%=%999XX'
  endif
  return s
endfunction
set tabline=%!Tabline()

if has("nvim")
    set scrollback=100000
endif

" useful mappings for for tabs and split screens
if has("nvim")
  map <F5> <ESC>:tabe\|term<CR>
else
  map <F5> <ESC>:shell<CR>
endif
" map <F6> <ESC>:tabe<Space>
map <F6> <ESC>:Tabedit<Space>
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
map <F10> <ESC>:NERDTreeToggle<CR>

if has("nvim")
  imap <F5> <ESC>:tabe\|term<CR>
else
  imap <F5> <ESC>:shell<CR>
endif
" imap <F6> <ESC>:tabe<Space>
imap <F6> <ESC>:Tabedit<Space>
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
imap <F10> <ESC>:NERDTreeToggle<CR>

if has("nvim")
  tnoremap <C-Down> <C-\><C-n>
  tnoremap <F5> <C-\><C-n>:tabe\|term<CR>
  tnoremap <F6> <C-\><C-n>:Tabedit<Space>
  tnoremap <F7> <C-\><C-n>:split<Space>
  tnoremap <F8> <C-\><C-n>:vsplit<Space>
  tnoremap <C-p> <C-\><C-n>"+p<ESC>i
  tnoremap <C-y> "+y
endif

command! -nargs=* -complete=file Tabedit call Tabedit(<f-args>)

function! Tabedit(...)
    if a:0 == 0
        tab split
    else
        for fname in a:000
            execute "tabe " . fname
        endfor
    endif
endfunction

" Disable arrow keys
inoremap <Up> <nop>
inoremap <Down> <nop>
inoremap <Left> <nop>
inoremap <Right> <nop>
noremap <Up> <nop>
noremap <Down> <nop>
noremap <Left> <nop>
noremap <Right> <nop>

" Resume where you left off
if has("autocmd")
    au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
endif

" No line numbers in terminal
if has("nvim")
    au TermOpen * setlocal nonumber norelativenumber
endif

" Cite as you write
noremap <F3> a<C-r>=ZoteroCite()<CR><ESC>
inoremap <F3> <C-r>=ZoteroCite()<CR>

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
command! RemoveTrailingWhitespace %s/\s\+$// | nohlsearch

" Change colorscheme ... should always be at the end
colorscheme desert

set cursorline
highlight CursorLine cterm=none ctermbg=233

highlight Pmenu ctermbg=black ctermfg=white guibg=blue guifg=white
highlight SpellBad ctermbg=red ctermfg=yellow guibg=red guifg=yellow
highlight Search ctermbg=darkblue ctermfg=yellow cterm=bold
highlight VertSplit ctermbg=black ctermfg=black

" disable autoindent for latex
autocmd FileType tex setlocal indentexpr=

" latex compilation
command! LatexClean execute "silent !rm -f /tmp/%:r.log /tmp/%:r.aux %:r.pdf" | redraw!
command! LatexDisplay execute "silent !xdg-open %:r.pdf > /dev/null 2>&1 &" | redraw!
command! LatexBibtex execute "silent !bibtex /tmp/%:r.aux >> /tmp/%.compile.out" | redraw!

function! LatexCompileAsync(nobibtex)
    " If there is a makefile, do that... otherwise, do the normal thing.
    if filereadable('Makefile') || filereadable('makefile')
        if has("nvim")
            call jobstart("make > /tmp/%.compile.out", {"on_exit": "LatexDoneCb"})
        else
            call job_start(["make"], {"out_io": "file", "out_name": "/tmp/%.compile.out", "exit_cb": "LatexDoneCb"})
        endif
    else
        " Also do Bibtex compile if there is a .bib file available
        if empty(glob("*.bib")) || a:nobibtex
            let cb = "LatexDoneCb"
        else
            let cb = "LatexBibtexCb"
        endif

        if has("nvim")
            let cmd = "pdflatex -interaction=nonstopmode -output-directory /tmp/ "
                \ . expand("%") . " > /tmp/" . expand("%") . ".compile.out"
            call jobstart(cmd, {"on_exit": cb})
        else
            call job_start(["pdflatex", "-interaction=nonstopmode", "-output-directory", "/tmp/", expand("%")],
                \ {"out_io": "file", "out_name": "/tmp/". expand("%") . ".compile.out", "exit_cb": cb})
        endif
    endif
endfunction

function! LatexBibtexCb(...)
    silent !cp *.bib /tmp/ >> /tmp/%.compile.out
    silent !cd /tmp/ && bibtex %:r.aux >> /tmp/%.compile.out
    call LatexCompileAsync(1)
endfunction

function! LatexDoneCb(...)
    if empty(glob("./*.pdf")) && !filereadable('/tmp/' . expand('%:r') . ".pdf")
        " No PDF produced
        echo "PDF not produced :("
        echo "Please see /tmp/" . expand('%') . ".compile.out for errors."
    elseif filereadable('/tmp/' . expand('%:r') . ".pdf")
        silent !mv /tmp/%:r.pdf .
        echom "Done compiling LaTeX."
    else
        echom "Done compiling LaTeX."
    endif
endfunction

function! LatexClean()
    " remove artifacts
    silent !rm -f /tmp/%:r.log /tmp/%:r.aux %:r.pdf
endfunction

function! LatexDisplay()
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

function! ZoteroCite()
  let format = 'cite'
  let api_call = 'http://localhost:23119/better-bibtex/cayw?format='.format.'&brackets=1'
  let ref = system('curl -s '.shellescape(api_call))
  return ref
endfunction

" Automatically compile on write
autocmd BufReadPost *.tex call LatexClean()
autocmd BufWritePost *.tex call LatexCompileAsync(0)
