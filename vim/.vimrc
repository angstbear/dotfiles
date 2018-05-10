" Vim power
set nocompatible

" Colors
syntax enable
colorscheme solarized
set background=dark

" General settings
filetype on
set noerrorbells
set visualbell
"set number
set cursorline
set ruler
set hlsearch
set incsearch
set ignorecase
set smartcase
set esckeys                    " Allow cursor keys in insert mode
set backspace=indent,eol,start " Allow backspacing over everything in insert mode
set laststatus=2               " Always show status line
set nostartofline              " Don’t reset cursor to start of line when moving around.
set scrolloff=3                " Start scrolling three lines before the horizontal window border
set showmode                   " Show current mode
set showmatch                  " Show matching brackets
set title                      " Show the filename in the window titlebar
set showcmd                    " Show the (partial) command as it’s being typed
set wildmenu                   " Command-line completion

" Tab spacing
set tabstop=4
set shiftwidth=4
set expandtab " Insert spaces on <TAB> press

" Maps backspace to clear last search highlighting
" turned off by default incase you like backspace, <BS>
nmap <silent> <BS> :nohlsearch<CR>

" Make `fd` throw you into normal mode
inoremap fd <esc>

let mapleader=","

" Toggle tabs / spaces with <TAB> press
nnoremap <leader>t :set invexpandtab<CR>
" Toggle line numbers
nnoremap <leader>l :set invnumber<CR>
" Save a file as root (,W)
nnoremap <leader>W :w !sudo tee % > /dev/null<CR>

" Strip trailing whitespace (,ws)
function! StripWhitespace()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    :%s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfunction
nnoremap <leader>ws :call StripWhitespace()<CR>

" Toggle Mouse support
function! ToggleMouse()
    if &mouse == 'a'
        set mouse=  " disable
    else
        set mouse=a " enable (all)
    endif
endfunction
nnoremap <leader>m :call ToggleMouse()<CR>

" Treat .json files as .js
autocmd BufNewFile,BufRead *.json setfiletype json syntax=javascript
" Treat .md files as Markdown
autocmd BufNewFile,BufRead *.md setlocal filetype=markdown

augroup perl
    " autoindent
    autocmd FileType perl set autoindent
    autocmd FileType perl set smartindent

    " make lines longer than 120 characters errors (including newline)
    autocmd FileType perl match ErrorMsg /\%>119v.\+/

    " make tabs and trailing spaces errors
    autocmd FileType perl 2match ErrorMsg /[\t]\|\s\+\%#\@<!$/

augroup END

" Status line (slightly more informative than default)
set statusline=%f\ [%{strlen(&fenc)?&fenc:'none'},%{&ff}]\ %h%m%r\ %y%=%c,%l/%L\ %P

