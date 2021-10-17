require('config.plugins')


--""""""""""""""""""""""""""""""""""""""""""
--"           NVIM Settings                "
--""""""""""""""""""""""""""""""""""""""""""

local set = vim.opt
set.completeopt = { 'menuone', 'noinsert', 'noselect' }
set.backspace = { 'indent', 'eol', 'start' }
set.shiftwidth = 2
set.tabstop = 2
set.smartindent = true
set.ignorecase = true
set.title = true
set.splitbelow = true
set.splitright = true
set.wrap = false
set.hlsearch = false
set.showmode = false
set.laststatus = 2
set.cmdheight = 2
set.number = true
set.relativenumber = true
set.signcolumn = 'yes'
set.updatetime = 300
set.mouse = 'a'
set.autoread = true
set.swapfile = false
set.backup = false
set.undofile = true
set.clipboard = 'unnamedplus'

set.termguicolors = true
set.cursorline = true
vim.cmd[[
	colorscheme gruvbox
	highlight Normal guibg=NONE
	highlight SignColumn guibg=NONE
	highlight CursorLine guibg=NONE
	highlight Search guibg=#91AFEB guifg=#666C87
	highlight VertSplit guibg=NONE guifg=#444444
	highlight Visual guibg=#666C87 guifg=#91AFEB gui=NONE
]]


--""""""""""""""""""""""""""""""""""""""""""
--"            Plugin Settings             "
--""""""""""""""""""""""""""""""""""""""""""

require('config.maps')
require('config.lsp')
require('config.cmp')
require('config.treesitter')
require('config.lualine')
require('config.bufferline')
require('config.telescope')
require('config.nvim-tree')

vim.cmd [[

	imap <BS> <Plug>(PearTreeBackspace)
	let g:pear_tree_smart_openers = 1
	let g:pear_tree_smart_closers = 1
	let g:pear_tree_smart_backspace = 1

	let g:pear_tree_map_special_keys = 0

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

]]


--""""""""""""""""""""""""""""""""""""""""""
--"              Functions                 "
--""""""""""""""""""""""""""""""""""""""""""

local au = require('utils.au')

-- Automatically deletes all trailing whitespace and newlines at end of file on save.
au.BufWritePre = function()
	vim.cmd('%s/\\s\\+$//e')
	vim.cmd('%s/\\n\\+\\%$//e')
end

-- Disables automatic commenting on newline:
au.FileType = function()
	vim.cmd('setlocal formatoptions-=c formatoptions-=r formatoptions-=o')
end
