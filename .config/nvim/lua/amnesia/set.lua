local set = vim.opt
-- set.completeopt = { 'menu', 'menuone', 'noselect' }
set.backspace = { 'indent', 'eol', 'start' }
set.fillchars:append { eob = " " }
set.list = true
set.listchars = { eol = "↲" , tab = "» ", trail = "·"}
set.scrolloff = 4
set.shiftwidth = 4
set.tabstop = 4
-- set.expandtab = true
-- set.smartindent = true
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
set.updatetime = 50
set.mouse = 'a'
set.mousefocus = true
set.mousemodel = 'extend'
set.autoread = true
set.swapfile = false
set.backup = false
set.undodir = os.getenv("HOME") .. "/.local/share/nvim/undodir"
set.undofile = true
set.clipboard = 'unnamedplus'
set.termguicolors = true
set.cursorline = true
