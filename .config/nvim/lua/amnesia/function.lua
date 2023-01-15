local autocmd = require('amnesia.autocmd')

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

-- Highlight yanked text
autocmd.TextYankPost = function ()
  vim.highlight.on_yank{on_visual=false}
end


