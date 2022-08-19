--""""""""""""""""""""""""""""""""""""""""""
--"           NVIM Settings                "
--""""""""""""""""""""""""""""""""""""""""""


local set = vim.opt
set.guifont = "Source Code Pro:h14"
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
require('config.dap')
require('config.lualine')
require('config.bufferline')
require('config.telescope')
require('config.nvim-tree')
require('config.toggleterm')
require('config.alpha')
require('config.peartree')
require('config.indent_blankline')
require('config.symbols')
require('config.comment')

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


require('filetype').setup({
    overrides = {
        extensions = {
            vert = "glsl",
            frag = "glsl",
        }
    }
})

vim.opt.runtimepath:append("~/Development/ezbookmarks.nvim")
require('ezbookmarks').setup{
    cwd_on_open = 1,
    use_bookmark_dir = 1,
    open_new_tab = 0,
}

require("which-key").setup {}

package.loaded[ "bop" ] = nil
vim.opt.runtimepath:append("~/development/bop.nvim")

vim.cmd[[
    colorscheme palenight
    highlight CursorLineNr guifg=#FABD2F
    highlight CursorLine guibg=#202230
    highlight Search guibg=#515873 guifg=#EDA36D gui=NONE
    highlight IndentBlanklineChar guifg=#3C4050
    highlight IndentBlanklineContextChar guifg=#51566B
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
autocmd.BufEnter = function()
	vim.cmd("silent! lcd %:p:h")
end

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
