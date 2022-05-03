--""""""""""""""""""""""""""""""""""""""""""
--"           NVIM Settings                "
--""""""""""""""""""""""""""""""""""""""""""


local set = vim.opt
set.guifont = "Source Code Pro:h13"
set.completeopt = { 'menu', 'menuone', 'noselect' }
set.backspace = { 'indent', 'eol', 'start' }
set.fillchars:append { eob = " " }
set.fillchars:append('vert:▕')
set.list = true
set.listchars = { eol = "↲" , tab = "» ", trail = "·"}
set.shiftwidth = 4
set.tabstop = 4
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
require('config.peartree')
require('config.indent_blankline')
require('config.symbols')
-- require('fFHighlight').setup()

require('colorizer').setup()
require('trld').setup()

require("fidget").setup{
    text = {
        spinner = "arc"
    },
    window = {
        blend = 0
    }
}

require('commented').setup{
   keybindings = {n = "<leader>cc", v = "<leader>cc", nl = "<leader>cc"}
}

vim.opt.runtimepath:append("~/development/ezbookmarks.nvim")
require('ezbookmarks').setup{
    cwd_on_open = 1,
    use_bookmark_dir = 1,
    open_new_tab = 0,
}

package.loaded[ "bop" ] = nil
vim.opt.runtimepath:append("~/development/bop.nvim")


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
  highlight NvimTreeCursorLine guibg=NONE guifg=#91AFEB
  highlight NvimTreeWindowPicker guibg=NONE guifg=#91AFEB
  highlight Pmenu guibg=#2B2E37
  highlight PmenuSel guibg=#91AFEB
  highlight PmenuSbar guibg=#2B2E37
  highlight PmenuThumb guibg=#515873
  highlight CmpItemAbbrDefault guifg=#AFA58A
  highlight CmpItemAbbrMatch guifg=#91AFEB
  highlight CmpItemAbbrMatchFuzzy guifg=#91AFEB
  highlight CmpItemKindDefault guifg=#AFAFAF
  highlight CmpItemMenuDefault guifg=#AFAFAF
  highlight NormalFloat guifg=#91AFEB guibg=None
  highlight FloatBorder guifg=#91AFEB guibg=None
  highlight TelescopeBorder guifg=#91AFEB
  highlight TelescopeMatching guifg=#EDA36D guibg=none
  highlight TelescopeSelection guifg=#91AFEB guibg=none
  highlight StatusLine guifg=NONE guibg=#2B2E37 gui=NONE
  highlight StatusLineNC guifg=NONE guibg=#2B2E37 gui=NONE
  highlight IndentBlanklineChar guifg=#3C4050
  highlight IndentBlanklineContextChar guifg=#51566B
  highlight FocusedSymbol guibg=NONE guifg=#91AFEB gui=bold
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

autocmd.VimEnter = function ()
  vim.cmd[[silent exec "!kill -s SIGWINCH $PPID"]]
end

-- cd buffer directory on enter
-- autocmd.BufEnter = function()
	-- vim.cmd("silent! lcd %:p:h")
-- end

-- autocmd.CursorHold = function()
  -- vim.cmd("let @/ = '\\V\\<'.escape(expand('<cword>'), '\\').'\\>'")
-- end
vim.cmd[[
  augroup terminal_settings
    autocmd!

    autocmd BufWinEnter,WinEnter term://* startinsert
    autocmd BufLeave term://* stopinsert

    " Ignore various filetypes as those will close terminal automatically
    " Ignore fzf, ranger, coc
    autocmd TermClose term://*
          \ if (expand('<afile>') !~ "fzf") && (expand('<afile>') !~ "ranger") && (expand('<afile>') !~ "coc") |
          \   call nvim_input('<CR>')  |
          \ endif
  augroup END
  ]]
