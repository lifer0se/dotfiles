

--""""""""""""""""""""""""""""""""""""""""""
--"           NVIM Settings                "
--""""""""""""""""""""""""""""""""""""""""""

local set = vim.opt
set.completeopt = { 'menuone', 'noinsert', 'noselect' }
set.backspace = { 'indent', 'eol', 'start' }
set.list = true
set.listchars = { eol = "↲" , tab = "» ", trail = "·"}
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
require('config.toggleterm')
require('colorizer').setup()
require('alpha').setup(require'alpha.themes.startify'.opts)
require('commented').setup{
	keybindings = {n = "<leader>cc", v = "<leader>cc", nl = "<leader>cc"}
}
-- require('ezbookmarks').setup()

vim.g.pear_tree_smart_openers = 1
vim.g.pear_tree_smart_closers = 1
vim.g.pear_tree_map_special_keys = 0
vim.g.pear_tree_ft_disabled = { "TelescopePrompt" }


vim.cmd[[
	colorscheme gruvbox
	highlight Normal guibg=NONE
	highlight SignColumn guibg=NONE
	highlight CursorLine guibg=NONE
	highlight CursorLineNr guibg=NONE
	highlight Search guibg=#91AFEB guifg=#515873
	highlight VertSplit guibg=NONE guifg=#444444
	highlight Visual guibg=#515873 guifg=#91AFEB gui=NONE
	highlight DiagnosticError guifg=#A30600
	highlight DiagnosticWarning guifg=#FABD2F
	highlight DiagnosticHint guifg=#689D6A
	highlight NvimTreeCursorLine guibg=#8094B4 guifg=#223E69
]]

--""""""""""""""""""""""""""""""""""""""""""
--"              Functions                 "
--""""""""""""""""""""""""""""""""""""""""""

local autocmd = require('utils.au')

-- Automatically deletes all trailing whitespace and newlines at end of file on save.
autocmd.BufWritePre = function()
	vim.cmd('%s/\\s\\+$//e')
	vim.cmd('%s/\\n\\+\\%$//e')
end

-- Disables automatic commenting on newline:
autocmd.FileType = function()
	vim.cmd('setlocal formatoptions-=c formatoptions-=r formatoptions-=o')
end

-- cd buffer directory on enter
-- autocmd.BufEnter = function()
	-- vim.cmd("silent! lcd %:p:h")
-- end
