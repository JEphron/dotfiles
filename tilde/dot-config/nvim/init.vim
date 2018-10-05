set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath

" Leader
let mapleader = " "

call plug#begin('~/.vim/plugged')
" useful stuff
Plug 'tpope/vim-surround'
Plug 'ctrlpvim/ctrlp.vim'

" langs
Plug 'sophacles/vim-processing'
Plug 'dag/vim-fish'
Plug 'rust-lang/rust.vim'
Plug 'reasonml-editor/vim-reason-plus'
Plug 'ElmCast/elm-vim'
Plug 'cespare/vim-toml'
Plug 'zah/nim.vim/'

" misc
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
Plug 'junegunn/fzf'
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
call plug#end()

" set tab width to 4 spaces
set tabstop=4
set shiftwidth=4

" wrap after 80 characters
set textwidth=80

" shortcut for run
" nnoremap <C-p> :silent make\|redraw!\|cw<CR>

" stupid thinkpad keyboard layout
inoremap <F1> <Esc>

" enable syntax coloring
syntax on

" remove ugly background color in gutter
highlight clear SignColumn

" buffer stuff (languageClient needs this) 
set hidden

" Line numbers
set number

" language servers
let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ 'rust': ['rustup', 'run', 'nightly', 'rls'],
    \ }

nnoremap <silent> K :call LanguageClient#textDocument_hover()<CR>
nnoremap <silent> gd :call LanguageClient#textDocument_definition()<CR>
nnoremap <silent> <F2> :call LanguageClient#textDocument_rename()<CR>
nnoremap <silent> gf :call LanguageClient_textDocument_formatting()<cr>
set completefunc=LanguageClient#complete

" Hide the search highlight (':noh' for 'no highlight')
nnoremap <Leader>h :noh<CR>

" Reload the vimrc (':so' for 'source')
nnoremap <Leader>so :so $MYVIMRC<CR>

" nnoremap <buffer> <F9> :exec 'st python' %<CR>

let g:ctrlp_cmd = 'CtrlPMRU'

filetype plugin on
set omnifunc=syntaxcomplete#Complete

" show existing tab with 4 spaces width
set tabstop=4
" when indenting with '>', use 4 spaces width
set shiftwidth=4
" On pressing tab, insert 4 spaces
set expandtab

" visible tabs
set list 
set listchars=tab:>- 

" execute commands on visual block 
" ex: :[range]w ! <some shell command>
function! VisualCountWords() range
    let n = @n
    silent! normal gv"ny
    echo "Word count:" . system("echo '" . @n . "' | wc -w")
    let @n = n
    " bonus: restores the visual selection
    normal! gv
endfunction

xnoremap <F6> :call VisualCountWords()<CR>

" random python settings
au BufNewFile,BufRead *.py set tabstop=4
au BufNewFile,BufRead *.py set softtabstop=4
au BufNewFile,BufRead *.py set shiftwidth=4
au BufNewFile,BufRead *.py set textwidth=79
au BufNewFile,BufRead *.py set expandtab
au BufNewFile,BufRead *.py set autoindent
au BufNewFile,BufRead *.py set fileformat=unix

