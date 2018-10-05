let mapleader=" " "Leader is space
let maplocalleader=" " "LocalLeader is space

let g:python3_host_prog = '/Users/jordanephron/miniconda3/bin/python3'
let g:python_host_prog  = '/usr/bin/python2.7'

call plug#begin()

" Bracket magic
Plug 'kovisoft/paredit'

" insert and delete bracket pairs automatically
Plug 'jiangmiao/auto-pairs'

" File browser
Plug 'scrooloose/nerdtree'

" Fast file switcher
Plug 'ctrlpvim/ctrlp.vim'

" Keys to work with matched pairs of braces
Plug 'tpope/vim-surround'

" Kotlin language support
Plug 'udalov/kotlin-vim'

" Elm language support
Plug 'ElmCast/elm-vim'

" TOML markup language support
Plug 'cespare/vim-toml'

" Elixir language support
Plug 'elixir-editors/vim-elixir'

" ReasonML language support
Plug 'reasonml-editor/vim-reason-plus'

" Typescript language server support
Plug 'Quramy/tsuquyomi'

" Typescript syntax support
Plug 'leafgarland/typescript-vim'

Plug 'sbdchd/neoformat'

Plug 'autozimu/LanguageClient-neovim', {
            \ 'branch': 'next',
            \ 'do': 'bash install.sh',
            \ }

call plug#end()

set expandtab "convert tabs to spaces
set tabstop=4 "indent 4 spaces per tab
set shiftwidth=4
set softtabstop=4
set hidden "switch buffers without writing
set number "show line numbers
set hidden
set autoindent


"LanguageClient config
let g:LanguageClient_serverCommands = {
            \ 'reason': ['ocaml-language-server', '--stdio'],
            \ 'ocaml': ['ocaml-language-server', '--stdio'],
            \ 'javascript': ['/Users/jordanephron/.npm-global/bin/javascript-typescript-stdio'],
            \ 'typescript': ['/Users/jordanephron/.npm-global/bin/javascript-typescript-stdio'],
            \ }

"LanguageClient bindings
nnoremap <silent> gd :call LanguageClient_textDocument_definition()<cr>
nnoremap <silent> ff :call LanguageClient_textDocument_formatting()<cr>
nnoremap <silent> <cr> :call LanguageClient_textDocument_hover()<cr>
nnoremap <silent> <F2> :call LanguageClient_textDocument_rename()<cr>

"edit the vimrc
nmap <silent> <leader>cfv :e $MYVIMRC<CR>
"reload the vimrc
nmap <silent> <leader>so :so $MYVIMRC<CR>

"close buffer
nmap <silent> <leader>q :bd<CR>

"grabbed from https://github.com/mutewinter/dot_vim/blob/master/mappings.vim
noremap H ^
noremap L $
noremap ^ <nop>
noremap $ <nop>

"hide highlighting (from search)
nnoremap <Leader>h :noh<CR>

"find and replace using \s
nnoremap <Leader>s :%s/\<<C-r><C-w>\>//g<Left><Left>

"copy to system clipboard
vnoremap <Leader>p "*y
nnoremap <Leader>p "*y

"split vertical
nnoremap <c-w>\ :vs<CR>
"split horizontal
nnoremap <c-w>- :sp<CR>
"close current window
nnoremap <c-w>x :on<CR>

"ctrl-p buffer management
noremap <c-p> :CtrlPMRU<cr>
inoremap <c-p> <esc>:CtrlPMRU<cr>
let g:ctrlp_cmd = 'CtrlPMRU'
let g:ctrlp_working_path_mode = 'w'

"elm stuff
autocmd FileType elm nnoremap <buffer> <silent> ff :ElmFormat<cr><cr>

"javascript stuff
autocmd FileType javascript nnoremap <buffer> <silent> ff :Neoformat<cr>

autocmd FileType html nnoremap <buffer> <silent> ff :!tidy -mi -html -wrap 0 %<cr>call feedkeys("<CR>L")<cr>

"formatter
let g:neoformat_enabled_javascript = ['prettier']

"NERDTree stuff
"quit vim if NERDTree is the only buffer left
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
"open with C-n
map <C-n> :NERDTreeToggle<CR>

"colors
hi Search ctermfg=White
hi Search ctermbg=Black
