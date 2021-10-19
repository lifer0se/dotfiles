

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



--""""""""""""""""""""""""""""""""""""""""""
--"            Plugin Settings             "
--""""""""""""""""""""""""""""""""""""""""""

require('config.plugins')
require('config.maps')
require('config.lsp')
require('config.cmp')
require('config.treesitter')
require('config.lualine')
require('config.bufferline')
require('config.telescope')
require('config.nvim-tree')
require('colorizer').setup()
require('alpha').setup(require'alpha.themes.startify'.opts)
require('nvim-autopairs').setup{}
require('commented').setup{
	keybindings = {n = "<leader>cc", v = "<leader>cc", nl = "<leader>cc"}
}


vim.cmd[[
	colorscheme gruvbox
	highlight Normal guibg=NONE
	highlight SignColumn guibg=NONE
	highlight CursorLine guibg=NONE
	highlight CursorLineNr guibg=NONE
	highlight Search guibg=#91AFEB guifg=#515873
	highlight VertSplit guibg=NONE guifg=#444444
	highlight Visual guibg=#515873 guifg=#91AFEB gui=NONE
	highlight DiagnosticError guifg=#AB1E18
	highlight DiagnosticWarning guifg=#FABD2F
	highlight DiagnosticHint guifg=#689D6A
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
