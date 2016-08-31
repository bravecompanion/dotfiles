set nocompatible
set encoding=utf-8 "necessary for vim-airline special chars

syntax on
filetype plugin indent on

"Windows-specific settings
if has('win32') || has('win64')
    source $VIMRUNTIME/vimrc_example.vim
    source $VIMRUNTIME/mswin.vim
    behave mswin

    set diffexpr=MyDiff()
    function MyDiff()
      let opt = '-a --binary '
      if &diffopt =~ 'icase' | let opt = opt . '-i ' | endif
      if &diffopt =~ 'iwhite' | let opt = opt . '-b ' | endif
      let arg1 = v:fname_in
      if arg1 =~ ' ' | let arg1 = '"' . arg1 . '"' | endif
      let arg2 = v:fname_new
      if arg2 =~ ' ' | let arg2 = '"' . arg2 . '"' | endif
      let arg3 = v:fname_out
      if arg3 =~ ' ' | let arg3 = '"' . arg3 . '"' | endif
      let eq = ''
      if $VIMRUNTIME =~ ' '
        if &sh =~ "\<cmd"
          let cmd = '""' . $VIMRUNTIME . '\diff"'
          let eq = '"'
        else
          let cmd = substitute($VIMRUNTIME, ' ', '" ', '') . '\diff"'
        endif
      else
        let cmd = $VIMRUNTIME . '\diff'
      endif
      silent execute '!' . cmd . ' ' . opt . arg1 . ' ' . arg2 . ' > ' . arg3 . eq
    endfunction

    autocmd VimEnter * simalt ~x

    "if I'm on windows, I'm on gvim (move to gvimrc someday)
    set guioptions-=m  "remove menu bar
    set guioptions-=T  "remove toolbar
    set guioptions-=r  "remove right-hand scroll bar
    set guifont=Consolas:h10
    colorscheme zenburn_me

    "colors for changed lines in Sy and vimdiff etc.
    highlight DiffAdd           guifg=#00ff00 gui=bold
    highlight DiffDelete        guifg=#ff0000 gui=bold
    highlight DiffChange        guifg=#ffff00 gui=bold

    let g:airline_theme="dark"
else
    "indent guides can't detect colors on terminal
    let g:indent_guides_auto_colors = 0
    autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd  ctermbg=240
    autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=239

    "colors for changed lines in Sy and vimdiff etc.
    highlight DiffAdd           ctermfg=46  cterm=bold
    highlight DiffDelete        ctermfg=196 cterm=bold
    highlight DiffChange        ctermfg=226 cterm=bold

    set title      "sets the title of the xterm window
    colorscheme zenburn 
endif

set ts=4          "tab stop
set sw=4          "soft tab stop
set et            "emulate tabs
set number        "line numbers
set cursorline    "highlight cursor line
set hid           "hide rather than close
set wrap          "wrap lines
set linebreak     "break line on whitespace only
set ruler         "show file position
set autoindent    "indent automatically
set showmode      "show INSERT/VISUAL/etc
set tags=./tags;  "search up for tags file, until found

"========================== KEYMAPS =========================
"maps f4 to vimgrep selected text
nnoremap <F4> :execute "vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR>
"maps shift-f4 to vimgrep selected text in the arglist
nnoremap <S-F4> :execute "vimgrep /" . expand("<cword>") . "/j ##" <Bar> cw<CR>
"maps ,cd to cd to the current file directory and print working directory
nnoremap ,cd :cd %:p:h<CR>:pwd<CR>

"navigate up and down by rows (rather than lines)
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

"navigate buffers with ctrl+arrow
nnoremap <C-Left> :bp<cr>
nnoremap <C-Right> :bn<cr>

"toggle tagbar
nmap <S-Tab> :TagbarToggle<cr>

"schlepp.vim keymaps--allows dragging blocks of text around
vmap <unique> <up>    <Plug>SchleppUp
vmap <unique> <down>  <Plug>SchleppDown
vmap <unique> <left>  <Plug>SchleppLeft
vmap <unique> <right> <Plug>SchleppRight

"resize vertical splits
nnoremap + <C-W>>
nnoremap _ <C-W><
"====================== END KEYMAPS =========================

"show matching brackets
set showmatch
set mat=2      "bracket blink time (tenths of a sec)

"search
set ignorecase "default ignore case
set smartcase  "case sensitive if uppercase char(s) present
set incsearch  "begin search immediately
set hlsearch   "highlight all hits

"put splits in a consistent place
set splitbelow
set splitright

"configure wildmenu
set wildmenu
set wildmode=longest:full,full
set wildignore=*.o,*.pyc,*.swp

set noswapfile "don't keep a swap file
set nobackup
set nowritebackup

"configure indent guides plugin
let g:indent_guides_start_level = 1
let g:indent_guides_guide_size = 1
autocmd VimEnter * :IndentGuidesEnable

"signify (version control cues)
let g:signify_vcs_list = ['svn', 'git']
let g:signify_update_on_focusgained = 1
let g:signify_sign_delete            = '-'
let g:signify_sign_delete_first_line = '_'

"============== vim-airline plugin
let g:airline_powerline_fonts = 1

if !exists('g:airline_symbols')
    let g:airline_symbols = {}
endif

"airline symbols
let g:airline_left_sep = ''
let g:airline_left_alt_sep = ''
let g:airline_right_sep = ''
let g:airline_right_alt_sep = ''
let g:airline_symbols.branch = ''
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = 'L:C '
let g:airline#extensions#whitespace#symbol = '!'
let g:airline#extensions#whitespace#trailing_format = 'ts[%s]'

let g:airline_section_b = '%{fnamemodify(".",":p:h:t")}'
let g:airline_section_c = '%{fnamemodify(bufname(""),":t")}'
let g:airline#extensions#tabline#enabled = 1  "gives tab line for open buffers
let g:airline#extensions#tabline#buffer_idx_mode = 1 "shows buffer numbers in tab line
let g:airline#extensions#tabline#fnamemod = ':.' "makes tabline path relative to cwd
let g:airline#extensions#quickfix#quickfix_text = 'QUICKFIX LIST' " name for quickfix buffer
let g:airline#extensions#quickfix#location_text = 'LOCATION LIST' " name for location list buffer
nmap <leader>1 <Plug>AirlineSelectTab1
nmap <leader>2 <Plug>AirlineSelectTab2
nmap <leader>3 <Plug>AirlineSelectTab3
nmap <leader>4 <Plug>AirlineSelectTab4
nmap <leader>5 <Plug>AirlineSelectTab5
nmap <leader>6 <Plug>AirlineSelectTab6
nmap <leader>7 <Plug>AirlineSelectTab7
nmap <leader>8 <Plug>AirlineSelectTab8
nmap <leader>9 <Plug>AirlineSelectTab9
set laststatus=2
"============== end vim-airline plugin

set backspace=indent,eol,start "allow backspacing to go past original location

"make quickfix always open when populated
"autocmd QuickFixCmdPost [^l]* nested botright cwindow
"autocmd QuickFixCmdPost    l* nested botright lwindow

"maintain argument list like buffer list
autocmd BufAdd * argadd <afile>
autocmd BufDelete * argdelete <afile>

autocmd BufEnter *.m    compiler mlint "Matlab syntax checking with mlint

