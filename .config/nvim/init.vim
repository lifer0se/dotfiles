call plug#begin('~/.local/share/nvim/plugged')

Plug 'mhinz/vim-startify'
Plug 'mbbill/undotree'
Plug 'preservim/nerdcommenter'
Plug 'francoiscabrol/ranger.vim'
Plug 'tpope/vim-surround'
Plug 'aserebryakov/vim-todo-lists'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'

Plug 'ryanoasis/vim-devicons'
Plug 'morhetz/gruvbox'
Plug 'ap/vim-css-color'

Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'ThePrimeagen/harpoon'

Plug 'neovim/nvim-lspconfig'
Plug 'ray-x/lsp_signature.nvim'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
Plug 'hrsh7th/cmp-path'
Plug 'hrsh7th/cmp-nvim-lua'
Plug 'onsails/lspkind-nvim'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

call plug#end()


""""""""""""""""""""""""""""""""""""""""""
"            VIM Settings                "
""""""""""""""""""""""""""""""""""""""""""

filetype indent plugin on
syntax enable

set completeopt=menuone,noinsert,noselect,preview
set backspace=indent,eol,start
set ignorecase
set shiftround
set shiftwidth=4
set softtabstop=-1
set tabstop=4
set formatoptions-=tc
set title
set smartindent
set shortmess+=c

set wildmenu
set hidden
set nofixendofline
set nostartofline
set splitbelow
set splitright

set incsearch
set laststatus=2
set relativenumber
set number
set nohlsearch
set nowrap
set noshowmode
set signcolumn=yes
set showcmd
set cmdheight=2

set noswapfile
set nobackup
set undofile

set updatetime=300
set mouse=a
set autoread

set clipboard=unnamedplus

set cursorline
set background=dark
colorscheme gruvbox

hi SignColumn ctermbg=NONE
hi Normal ctermbg=NONE
hi CursorLine ctermbg=NONE
hi Search cterm=NONE ctermfg=189 ctermbg=8
hi VertSplit ctermbg=NONE ctermfg=black

autocmd User AirlineAfterInit hi airline_tabhid ctermbg=238 ctermfg=249
autocmd User AirlineAfterInit hi airline_tabsel ctermbg=238 ctermfg=150
autocmd User AirlineAfterInit hi airline_tabmod ctermbg=238 ctermfg=179


""""""""""""""""""""""""""""""""""""""""""
"            Plugin Settings             "
""""""""""""""""""""""""""""""""""""""""""

lua require('lsp')

let g:airline_theme='bubblegum'
let g:airline_extensions = ['branch', 'tabline']
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline_symbols_ascii = 1
let g:airline#extensions#tabline#left_sep = ' '
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
let g:airline_left_sep = ' '
let g:airline_left_alt_sep = ' '
let g:airline_right_sep = ' '
let g:airline_right_alt_sep = ' '
let g:airline_symbols.branch = ''
let g:airline_symbols.dirty='!'
let g:airline_symbols.readonly = ''
let g:airline_symbols.linenr = '  ☰ '
let g:airline_symbols.colnr = ':'
let g:airline_symbols.maxlinenr = ' '
let g:airline#extensions#tabline#keymap_ignored_filetypes =
        \ ['vimfiler', 'nerdtree']

nnoremap <silent> ` :lua require("harpoon.mark").add_file()<cr>
nnoremap <silent> ~ :lua require("harpoon.ui").toggle_quick_menu()<cr>
nnoremap <silent> ! :lua require("harpoon.ui").nav_file(1)<cr>
nnoremap <silent> @ :lua require("harpoon.ui").nav_file(2)<cr>
nnoremap <silent> # :lua require("harpoon.ui").nav_file(3)<cr>
nnoremap <silent> $ :lua require("harpoon.ui").nav_file(4)<cr>

let g:ranger_command_override = 'ranger --cmd "set show_hidden=true"'

let g:startify_files_number = 18
let g:startify_custom_indices = map(range(1,100), 'string(v:val)')
let g:startify_session_persistence = 1
let g:startify_lists = [
  \ { 'type': 'dir',       'header': ['   Recent files'] },
  \ { 'type': 'sessions',  'header': ['   Saved sessions'] },
  \ ]
let g:startify_custom_header = [
\'                                ',
\'  __   _ __      _______ __  __ ',
\' |  \ | |\ \    / /_   _|  \/  |',
\' | \ \| | \ \  / /  | | | \  / |',
\' | |\ \ |  \ \/ /   | | | |\/| |',
\' | | \  |   \  /   _| |_| |  | |',
\' |_|  \_|    \/   |_____|_|  |_|',
\'                                ',
\'                                ']


""""""""""""""""""""""""""""""""""""""""""
"              Functions                 "
""""""""""""""""""""""""""""""""""""""""""

if ! has('gui_running')
    set ttimeoutlen=10
    augroup FastEscape
        autocmd!
        au InsertEnter * set timeoutlen=0
        au InsertLeave * set timeoutlen=1000
    augroup END
endif

" Automatically deletes all trailing whitespace and newlines at end of file on save.
autocmd BufWritePre * %s/\s\+$//e
autocmd BufWritePre * %s/\n\+\%$//e

" Disables automatic commenting on newline:
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Save file as sudo on files that require root permission
cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

command! Vimrc :edit ~/.config/nvim/init.vim
augroup reload_vimrc
  au!
  au BufWritePost,FileWritePost *.vim,~/.vimrc,~/.config/nvim/init.vim source <afile>
augroup END


""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""" Remaps """"""""""""""""""
""""""""""""""""""""""""""""""""""""""""""

let mapleader = "\<Space>"

nnoremap <silent> <leader>gg :G<cr>
nnoremap <leader>gc :Git commit<cr>
nnoremap <leader>gp :Git push<cr>

nnoremap <C-s> :w<cr>
inoremap <C-s> <esc>
vnoremap <C-s> <C-c>:w<cr>

nnoremap <leader>u :UndotreeToggle<cr>

nnoremap Y y$
noremap <leader>p "_dP
noremap <leader>P "_dp
vnoremap <leader>p "_dP
vnoremap <leader>P "_dp

map <leader>tn :tabnew<cr>
nmap L <Plug>AirlineSelectNextTab
nmap H <Plug>AirlineSelectPrevTab

noremap <silent> <C-q> :bd!<cr>
noremap <C-c> <C-w>c
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap = <C-w>5>
noremap - <C-w>5<

nnoremap Q @q
vnoremap Q :norm @q<cr>

nmap >> <Nop>
nmap << <Nop>
vmap >> <Nop>
vmap << <Nop>
nnoremap <Tab>   >>
nnoremap <S-Tab> <<
vnoremap <Tab>   >><Esc>gv
vnoremap <S-Tab> <<<Esc>gv

noremap <leader>h :set hlsearch! hlsearch?<cr>

nnoremap <leader>sc :%s///gc<Left><Left><Left><Left>
nnoremap <leader>ss :%s///g<Left><Left><Left>
vnoremap <leader>sc y:%s/<C-R>"//gc<Left><Left><Left><C-R>"
vnoremap <leader>ss y:%s/<C-R>"//g<Left><Left><C-R>"

nnoremap <silent> <2-LeftMouse> viw
nnoremap <silent> <C-f> /<C-R>=escape(expand("<cWORD>"), "/")<cr><cr>:set hls<cr>
vnoremap <silent> <C-f> y0/<C-r>"<cr>:set hls<cr>

map <silent> <leader>t :vsplit <C-R>=expand("%:p:h") . "/" <CR>list.todo.md<CR><C-w>30<

nnoremap n nzzzv
nnoremap N Nzzzv

inoremap , ,<c-g>u
inoremap . .<c-g>u
inoremap ! !<c-g>u
inoremap ? ?<c-g>u
inoremap ' '<c-g>u
inoremap " "<c-g>u
inoremap ` `<c-g>u
inoremap < <<c-g>u
inoremap > ><c-g>u
inoremap [ [<c-g>u
inoremap ] ]<c-g>u
inoremap { {<c-g>u
inoremap } }<c-g>u
inoremap ( (<c-g>u
inoremap ) )<c-g>u

vnoremap <silent> J :m '>+1<cr>gv=gv
vnoremap <silent> K :m '<-2<cr>gv=gv
nnoremap <silent> J :m .+1<cr>==
nnoremap <silent> K :m .-2<cr>==