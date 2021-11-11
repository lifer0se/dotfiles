--""""""""""""""""""""""""""""""""""""""""""
--"           NVIM Settings                "
--""""""""""""""""""""""""""""""""""""""""""


local set = vim.opt
set.completeopt = { 'menu', 'menuone', 'noselect' }
set.backspace = { 'indent', 'eol', 'start' }
set.list = true
set.listchars = { eol = "↲" , tab = "» ", trail = "·"}
set.shiftwidth = 2
set.tabstop = 2
set.expandtab = true
set.smartindent = true
set.ignorecase = true
set.title = true
set.splitbelow = true
set.splitright = true
set.wrap = false
set.hlsearch = true
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
require('config.lspkind')
require('config.cmp')
require('config.treesitter')
require('config.lualine')
require('config.bufferline')
require('config.telescope')
require('config.nvim-tree')
require('config.toggleterm')
require('config.alpha')

require("stabilize").setup()
require('mkdir')
require('colorizer').setup()
require('commented').setup{
 keybindings = {n = "<leader>cc", v = "<leader>cc", nl = "<leader>cc"}
}

vim.opt.runtimepath:append("~/development/ezbookmarks.nvim")
require('ezbookmarks').setup{
  cwd_on_open = 1,
  use_bookmark_dir = 1,
  open_new_tab = 0,
}

vim.cmd[[
  colorscheme gruvbox
  highlight Normal guibg=NONE
  highlight SignColumn guibg=NONE
  highlight CursorLine guibg=NONE
  highlight CursorLineNr guibg=NONE guifg=#FABD2F
  highlight Search guibg=#515873 guifg=#EDA36D gui=NONE
  highlight VertSplit guibg=NONE guifg=#2B2E37
  highlight Visual guibg=#515873 guifg=#91AFEB gui=NONE
  highlight ErrorMsg guibg=NONE guifg=#cc241d gui=bold
  highlight DiagnosticError guifg=#cc241d guibg=NONE
  highlight DiagnosticWarning guifg=#FABD2F guibg=NONE
  highlight DiagnosticHint guifg=#688D6A guibg=NONE
  highlight NvimTreeCursorLine guibg=#515873 guifg=#91AFEB
  highlight Pmenu guibg=#2B2E37
  highlight PmenuSel guibg=#91AFEB
  highlight PmenuSbar guibg=#2B2E37
  highlight PmenuThumb guibg=#515873
  highlight CmpItemAbbrDefault guifg=#AFA58A
  highlight CmpItemAbbrMatch guifg=#91AFEB
  highlight CmpItemAbbrMatchFuzzy guifg=#91AFEB
  highlight CmpItemKind guifg=#AFAFAF
  highlight CmpItemMenuDefault guifg=#AFAFAF
  highlight NormalFloat guifg=#358292 guibg=None
  highlight FloatBorder guifg=#358292 guibg=None
  highlight TelescopeBorder guifg=#358292
  highlight TelescopeMatching guifg=#EDA36D
  highlight clear StatusLine
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
local function remove_autocomment()
	vim.cmd('setlocal formatoptions-=c formatoptions-=r formatoptions-=o')
end
autocmd.BufNew = function()
  remove_autocomment()
end
autocmd.BufRead = function()
  remove_autocomment()
end

autocmd.TextYankPost = function ()
  vim.highlight.on_yank{on_visual=false}
end

-- cd buffer directory on enter
-- autocmd.BufEnter = function()
	-- vim.cmd("silent! lcd %:p:h")
-- end

-- autocmd.CursorHold = function()
  -- vim.cmd("let @/ = '\\V\\<'.escape(expand('<cword>'), '\\').'\\>'")
-- end
