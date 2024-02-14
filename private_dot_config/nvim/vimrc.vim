filetype plugin indent on

" Enable mouse
set mouse=a
set tabpagemax=50

set number

syntax on

" Indent automatically depending on filetype
filetype indent on
filetype plugin on
set autoindent

set shiftround

set wildmenu
set wildmode=longest,list

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

"set background=dark
"set t_Co=256
colorscheme kanagawa

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
set statusline+=%#warningmsg#
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
nmap <silent> <leader>a :A<CR>
nnoremap <leader>. :CtrlPTag<cr>


" Hilight matching parenthesis
hi MatchParen cterm=NONE ctermbg=red ctermfg=blue
hi Normal ctermbg=234

" Fix highlighting for large js files
autocmd BufEnter *.{js,jsx,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{js,jsx,ts,tsx} :syntax sync clear

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

au FileType c setl shiftwidth=8
au FileType c setl tabstop=8
au FileType c setl softtabstop=8
au FileType c setl noexpandtab

au FileType cpp setl shiftwidth=8
au FileType cpp setl tabstop=8
au FileType cpp setl softtabstop=8
au FileType cpp setl noexpandtab

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

au FileType cmake setl shiftwidth=2
au FileType cmake setl tabstop=2
au FileType cmake setl softtabstop=2
au FileType cmake setl expandtab

au BufNewFile,BufRead *.ttcn setf ttcn
au FileType ttcn setl shiftwidth=2
au FileType ttcn setl tabstop=2
au FileType ttcn setl softtabstop=2
au FileType ttcn setl expandtab

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

au BufNewFile,BufRead *.gradle set filetype=groovy
au FileType groovy setl tabstop=4
au FileType groovy setl softtabstop=4
au FileType groovy setl shiftwidth=4
au FileType groovy setl expandtab

au FileType markdown setl tabstop=4
au FileType markdown setl softtabstop=4
au FileType markdown setl shiftwidth=4
au FileType markdown setl expandtab

au FileType json setl tabstop=4
au FileType json setl softtabstop=4
au FileType json setl shiftwidth=4
au FileType json setl expandtab

au FileType bzl setl tabstop=4
au FileType bzl setl softtabstop=4
au FileType bzl setl shiftwidth=4
au FileType bzl setl expandtab

" Use ansible-yaml plugin for YAML syntax highlighting and indenting
"au BufNewFile,BufRead *.yml,*.yaml set filetype=ansible

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

nnoremap <Tab> :bnext<CR>
nnoremap <S-Tab> :bprevious<CR>
nnoremap <C-X> :bdelete<CR>

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

let g:syntastic_c_include_dirs = ['src', 'src/include', 'src/korn', 'platform/include', 'platform/_os/linux', 'platform/_arch', 'build_media']
let g:syntastic_c_compiler_options = '-DSP_DEBUG=1'

" Unite
"let g:unite_source_history_yank_enable = 1
"call unite#filters#matcher_default#use(['matcher_fuzzy'])
"nnoremap <leader>t :<C-u>Unite -no-split -buffer-name=files   -start-insert file_rec/async:!<cr>
"nnoremap <leader>f :<C-u>Unite -no-split -buffer-name=files   -start-insert file<cr>
"nnoremap <leader>r :<C-u>Unite -no-split -buffer-name=mru     -start-insert file_mru<cr>
"nnoremap <leader>o :<C-u>Unite -no-split -buffer-name=outline -start-insert outline<cr>
"nnoremap <leader>y :<C-u>Unite -no-split -buffer-name=yank    history/yank<cr>
"nnoremap <leader>e :<C-u>Unite -no-split -buffer-name=buffer  buffer<cr>
"
"" Custom mappings for the unite buffer
"autocmd FileType unite call s:unite_settings()
"function! s:unite_settings()
"  " Play nice with supertab
"  let b:SuperTabDisabled=1
"  " Enable navigation with control-j and control-k in insert mode
"  imap <buffer> <C-j>   <Plug>(unite_select_next_line)
"  imap <buffer> <C-k>   <Plug>(unite_select_previous_line)
"endfunction

" Set text width for email messages written in mutt
au BufRead /tmp/mutt-* set tw=72

" No popup for Python autocompletion
let g:jedi#popup_on_dot = 0

" Use rainbow parenthesis always
"au VimEnter * RainbowParenthesesToggle
"au Syntax * RainbowParenthesesLoadRound
"au Syntax * RainbowParenthesesLoadSquare
"au Syntax * RainbowParenthesesLoadBraces
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['lightblue',   'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['yellow',      'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['lightblue',   'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]


highlight GitGutterAdd    ctermfg=green
highlight GitGutterChange ctermfg=yellow
highlight GitGutterDelete ctermfg=red

let g:ctrlp_open_multiple_files = '1'

let g:gutentags_trace = 1

let g:tagbar_ctags_bin = "/snap/bin/ctags"

let g:tagbar_type_elm = {
      \ 'kinds' : [
      \ 'f:function:0:0',
      \ 'm:modules:0:0',
      \ 'i:imports:1:0',
      \ 't:types:1:0',
      \ 'a:type aliases:0:0',
      \ 'c:type constructors:0:0',
      \ 'p:ports:0:0',
      \ 's:functions:0:0',
      \ ]
      \}

" if hidden is not set, TextEdit might fail.
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Better display for messages
set cmdheight=2

" You will have bad experience for diagnostic messages when it's default 4000.
set updatetime=300

" don't give |ins-completion-menu| messages.
set shortmess+=c

" always show signcolumns
set signcolumn=yes

au FileType lisp call rainbow#load()

" Use <c-space> to trigger completion.
inoremap <silent><expr> <c-space> coc#refresh()

" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current position.
" Coc only does snippet and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" Or use `complete_info` if your vim support it, like:
" inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"

" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" Remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Highlight symbol under cursor on CursorHold
autocmd CursorHold * silent call CocActionAsync('highlight')

" Remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" Remap for format selected region
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Remap for do codeAction of selected region, ex: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap for do codeAction of current line
nmap <leader>ac  <Plug>(coc-codeaction)
" Fix autofix problem of current line
nmap <leader>qf  <Plug>(coc-fix-current)

" Create mappings for function text object, requires document symbols feature of languageserver.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

" Use <C-d> for select selections ranges, needs server support, like: coc-tsserver, coc-python
"nmap <silent> <C-d> <Plug>(coc-range-select)
"xmap <silent> <C-d> <Plug>(coc-range-select)

" Use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" Use `:Fold` to fold current buffer
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" use `:OR` for organize import of current buffer
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')


command Bd bp\|bd \#

" Add status line support, for integration with other plugin, checkout `:h coc-status`
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Using CocList
" Show all diagnostics
nnoremap <silent> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions
nnoremap <silent> <space>e  :<C-u>CocList extensions<cr>
" Show commands
nnoremap <silent> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document
nnoremap <silent> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols
nnoremap <silent> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list
nnoremap <silent> <space>p  :<C-u>CocListResume<CR>
