set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#begin()

Plugin 'gmarik/vundle'

" Plugins
Plugin 'Arduino-syntax-file'
Plugin 'Glench/Vim-Jinja2-Syntax'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'Rykka/colorv.vim'
Plugin 'Shougo/neocomplcache'
Plugin 'Shougo/unite.vim'
Plugin 'Twinside/vim-haskellFold'
Plugin 'Twinside/vim-hoogle'
Plugin 'a.vim'
Plugin 'adinapoli/cumino'
Plugin 'airblade/vim-gitgutter'
Plugin 'chase/vim-ansible-yaml'
Plugin 'confluencewiki.vim'
Plugin 'dag/vim2hs'
Plugin 'ehamberg/haskellmode-vim'
Plugin 'godlygeek/tabular'
Plugin 'goldfeld/vim-seek'
Plugin 'gregsexton/MatchTag'
Plugin 'gregsexton/gitv'
Plugin 'guns/vim-clojure-static'
Plugin 'haskell.vim'
Plugin 'hsanson/vim-android'
Plugin 'hspec/hspec.vim'
Plugin 'ivanov/vim-ipython'
Plugin 'jimenezrick/vimerl'
Plugin 'kien/ctrlp.vim'
Plugin 'kien/rainbow_parentheses.vim'
Plugin 'kovisoft/slimv'
Plugin 'majutsushi/tagbar'
Plugin 'mfukar/robotframework-vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'nelstrom/vim-visual-star-search'
Plugin 'pbrisbin/html-template-syntax'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/syntastic'
Plugin 'sjl/gundo.vim'
Plugin 'sjl/splice.vim'
Plugin 'tpope/vim-classpath'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-dispatch'
Plugin 'tpope/vim-fireplace'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-leiningen'
Plugin 'tpope/vim-markdown'
Plugin 'tpope/vim-projectionist'
Plugin 'tpope/vim-repeat'
Plugin 'tpope/vim-surround'
Plugin 'vimwiki'

" Colorschemes
Plugin 'sickill/vim-sunburst'
Plugin 'sickill/vim-monokai'
Plugin 'inkpot'
Plugin 'Wombat'
Plugin 'wombat256.vim'
Plugin 'nanotech/jellybeans.vim'

" Following plugins require manual installation or depend on other plugins
" req: manual installation
"Plugin 'Shougo/vimproc'
" req: manual installation
"Plugin 'wincent/Command-T'
" ref: manual installation
"Plugin 'Valloric/YouCompleteMe'
" req: vimproc, ghc-mod
"Plugin 'eagletmt/ghcmod-vim'
" req: ghc-mod
"Plugin 'ujihisa/neco-ghc'
" req: vimproc
"Plugin 'Shougo/vimshell'
" req: lushtags
"Plugin 'bitc/lushtags'
" req: jedi
"Plugin 'davidhalter/jedi-vim'

call vundle#end()
filetype plugin indent on

" Enable mouse
set mouse=a
set tabpagemax=50

syntax on

" Indent automatically depending on filetype
filetype indent on
filetype plugin on
set autoindent

set wildmenu
set linebreak
set showcmd
set incsearch

set history=1000

" Set region to American English
set spelllang=en_us

" Case insensitive search
set ic

" Highlight search
set hls

set background=dark
set t_Co=256
colorscheme inkpot

" No wrap
set textwidth=0
set fo-=t

" Use par program
set formatprg=par\ -w79

set showmode

" Statusline
set laststatus=2
set statusline=
set statusline+=%<\                       " cut at start
set statusline+=%2*[%n%H%M%R%W]%*\        " buffer number, and flags
set statusline+=%-40f\                    " relative path
set statusline+=%1*%{fugitive#statusline()}%*  " git status
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%=                        " seperate between right- and left-aligned
set statusline+=%1*%y%*%*\                " file type
set statusline+=%10((%l,%v/%L)%)\         " line and column
set statusline+=%P                        " percentage of file

set pastetoggle=<F2>

" disable ex mode
nmap Q q

" map ö and Ö to :
exe "nmap <Char-246> :"
exe "nmap <Char-214> :"

ino jj <esc>
cno jj <c-c>

" save file with root permissions by typing w!!
cmap w!! w !sudo tee % > /dev/null

" Buffer manipulation with UP ARROW and DOWN ARROW
nmap <special> <C-Up> :bprevious<CR>
nmap <special> <C-Down> :bnext<CR>

" C-N toggle line numbers, C-M toggle invisible symbols
nmap <silent><C-N><C-N> :set invnumber <CR>
nmap <silent><C-M><C-M> :set invlist <CR>

" Switch tabs with ALT+LEFT/RIGHT
map <silent><A-Right> :tabnext<CR>
map <silent><A-Left> :tabprevious<CR>

" Move between windows
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l

let maplocalleader='\'

" Open with current directory
let mapleader=','
map <leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
map <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
map <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
map <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

nmap <silent> <leader>l :set list!<CR>
nmap <silent> <leader>w :set wrap!<CR>
nmap <silent> <leader>s :set spell!<CR>
nmap <silent> <leader>n :silent :nohlsearch<CR>
nmap <silent> <leader>cd :cd %:p:h<CR>:pwd<CR>
nmap <silent> <leader>f :NERDTreeToggle<CR>
nmap <silent> <leader>a :A<CR>

" Hilight matching parenthesis
hi MatchParen cterm=NONE ctermbg=red ctermfg=black

" FileType settings

au FileType text setl tw=78
au FileType text setl fo+=t

au FileType lua setl tabstop=2
au FileType lua setl shiftwidth=2
au FileType lua setl softtabstop=2
au FileType lua setl expandtab

au FileType javascript setl tabstop=2
au FileType javascript setl shiftwidth=2
au FileType javascript setl softtabstop=2
au FileType javascript setl expandtab

au FileType tex setl tabstop=4
au FileType tex setl shiftwidth=4
au FileType tex setl softtabstop=4
au FileType tex setl expandtab

au FileType html,vim setl shiftwidth=4
au FileType html,vim setl tabstop=4
au FileType html,vim setl softtabstop=4

au FileType perl setl shiftwidth=4
au FileType perl setl tabstop=4
au FileType perl setl softtabstop=4
au FileType perl setl expandtab

au FileType python setl shiftwidth=4
au FileType python setl tabstop=4
au FileType python setl softtabstop=4
au FileType python setl expandtab

au FileType ruby setl shiftwidth=2
au FileType ruby setl tabstop=2
au FileType ruby setl softtabstop=2
au FileType ruby setl expandtab

au FileType java setl shiftwidth=2
au FileType java setl tabstop=2
au FileType java setl softtabstop=2
au FileType java setl expandtab

au FileType haskell setl shiftwidth=8
au FileType haskell setl tabstop=8
au FileType haskell setl softtabstop=8
au FileType haskell setl expandtab

au FileType cabal setl shiftwidth=2
au FileType cabal setl tabstop=2
au FileType cabal setl softtabstop=2
au FileType cabal setl expandtab

au FileType sh setl shiftwidth=4
au FileType sh setl tabstop=4
au FileType sh setl softtabstop=4
au FileType sh setl expandtab

au FileType cmake setl shiftwidth=4
au FileType cmake setl tabstop=4
au FileType cmake setl softtabstop=4
au FileType cmake setl expandtab

au BufNewFile,BufRead *.xsd	setf xsd
au BufNewFile,BufRead *.xsl	setf xsl
au BufNewFile,BufRead *.dtd	setf dtd
au FileType xml setl tabstop=2
au FileType xml setl textwidth=0
au FileType xml setl softtabstop=2
au FileType xml setl shiftwidth=2
au FileType xml setl expandtab
au FileType xsd setl tabstop=2
au FileType xsd setl textwidth=0
au FileType dtd setl tabstop=2
au FileType dtd setl textwidth=0
au FileType xslt setl tabstop=2
au FileType xslt setl textwidth=0

au BufNewFile,BufRead *.pde setlocal ft=arduino
au BufNewFile,BufRead *.ino setlocal ft=arduino

au BufNewFile,BufRead *.config setlocal ft=config

au BufNewFile,BufRead *.gdb setlocal ft=gdb

" Color trailing whitespace
highlight ExtraWhitespace ctermbg=red guibg=red
au ColorScheme * highlight ExtraWhitespace guibg=red
au BufEnter * match ExtraWhitespace /\s\+$/
au InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
au InsertLeave * match ExtraWhiteSpace /\s\+$/

" Whitespace chars
set listchars=eol:$,tab:>-,trail:-,extends:>,precedes:<

" vim-latex
" IMPORTANT: grep will sometimes skip displaying the file name if you
" search in a singe file. This will confuse Latex-Suite. Set your grep
" program to always generate a file-name.
set grepprg=grep\ -nH\ $*

" OPTIONAL: Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

" vim-commentary
" Use // instead of /* */ commenting in C and C++ files
autocmd FileType c set commentstring=//\ %s
autocmd FileType cpp set commentstring=//\ %s
autocmd FileType python set commentstring=#\ %s

" vim-easymotion
let g:EasyMotion_leader_key = '<Leader>m'

" Gundo
nnoremap <F10> :GundoToggle<CR>

" LaTeX conceal
set cole=2
let g:tex_conceal= 'adgm'
hi Conceal ctermbg=Black ctermfg=White

" Persistent undo
set undofile
set undodir=/tmp

" F3 and F4 Toggle column boundary
hi ColorColumn ctermbg=darkyellow
nmap <F4> :set colorcolumn=80,120 <CR>
nmap <F3> :set colorcolumn=0 <CR>

" Toggle Relative line numbers
nmap <F7> :set relativenumber <CR>
nmap <F6> :set norelativenumber <CR>

" TagBar
nmap <F8> :TagbarToggle <CR>

" Ghc-mod overlay color
hi ghcmodType ctermbg=yellow
let g:ghcmod_type_highlight = 'ghcmodType'

" neocomplcache plugin startup
let g:neocomplcache_enable_at_startup = 1

" haskellmode
" Use ghc functionality for haskell files
au Bufenter *.hs compiler ghc
let g:haddock_browser = "firefox"

au BufNewFile,BufRead Tupfile,*.tup set filetype=tup

" Disable arrow keys
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>
inoremap <Up> <NOP>
inoremap <Down> <NOP>
inoremap <Left> <NOP>
inoremap <Right> <NOP>

" Fix sign column background color
highlight clear SignColumn

au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
au BufEnter *.org            call org#SetOrgFileType()

" Load matchit.vim, but only if the user hasn't installed a newer version.
if !exists('g:loaded_matchit') && findfile('plugin/matchit.vim', &rtp) ==# ''
  runtime! macros/matchit.vim
endif

" Show detailed information (type) of Haskell symbols.
let g:necoghc_enable_detailed_browse = 1

" Fix backspace
set backspace=indent,eol,start

" Ignore warnings from too long lines in Python files
let g:syntastic_python_flake8_args = '--ignore="E501"'

" Unite
let g:unite_source_history_yank_enable = 1
call unite#filters#matcher_default#use(['matcher_fuzzy'])
nnoremap <leader>t :<C-u>Unite -no-split -buffer-name=files   -start-insert file_rec/async:!<cr>
nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files   -start-insert file<cr>
nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru     -start-insert file_mru<cr>
nnoremap <leader>o :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank    history/yank<cr>
nnoremap <leader>e :<C-u>Unite -no-split -buffer-name=buffer  buffer<cr>

" Custom mappings for the unite buffer
autocmd FileType unite call s:unite_settings()
function! s:unite_settings()
  " Play nice with supertab
  let b:SuperTabDisabled=1
  " Enable navigation with control-j and control-k in insert mode
  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
endfunction

" Set text width for email messages written in mutt
au BufRead /tmp/mutt-* set tw=72

" No popup for Python autocompletion
let g:jedi#popup_on_dot = 0

" Use rainbow parenthesis always
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces
