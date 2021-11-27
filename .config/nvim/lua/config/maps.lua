local map = function(key)
  local opts = {noremap = true}
  for i, v in pairs(key) do
    if type(i) == 'string' then opts[i] = v end
	end

	local buffer = opts.buffer
  opts.buffer = nil

  if buffer then
    vim.api.nvim_buf_set_keymap(0, key[1], key[2], key[3], opts)
  else
    vim.api.nvim_set_keymap(key[1], key[2], key[3], opts)
	end
end

vim.g.mapleader = " "
map { 'n', noremap = false, 'Q', '@q' }
map { 'n', noremap = false, '<C-s>', ':w<CR>' }
map { 'v', noremap = false, '<C-s>', ':w<CR>' }
map { 'i', noremap = false, '<C-s>', '<esc>' }
map { 'n', noremap = false, silent = true, '<leader>gg', ':G<CR>' }
map { 'n', noremap = false, '<leader>gc', ':Git commit<CR>' }
map { 'n', noremap = false, '<leader>gp', ':Git push<CR>' }
map { 'n', noremap = false, '<leader>u', ':UndotreeToggle<CR>' }
map { 'n', noremap = false, 'Y', 'y$' }
map { 'n', noremap = false, '<leader>p', '"_dP' }
map { 'n', noremap = false, '<leader>P', '"_dp' }
map { 'v', noremap = false, '<leader>p', '"_dP' }
map { 'v', noremap = false, '<leader>P', '"_dp' }
map { 'i', noremap = false, '<C-p>', '<C-R>+' }
map { 'n', noremap = false, '<leader>tn', ':tabnew<CR>' }
map { 'n', noremap = false, silent = true, '<C-z>', ':bd!<CR>' }
map { 'n', noremap = false, '<C-c>', '<C-w>c' }
map { 'n', noremap = false, '<C-h>', '<C-w>h' }
map { 'n', noremap = false, '<C-j>', '<C-w>j' }
map { 'n', noremap = false, '<C-k>', '<C-w>k' }
map { 'n', noremap = false, '<C-l>', '<C-w>l' }
map { 'n', noremap = true, silent = true, '<C-Up>', ':resize +2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Down>', ':resize -2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Left>', ':vertical resize +2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Right>',':vertical resize -2<CR>' }
map { 'n', noremap = false, '<Tab>', '>>' }
map { 'n', noremap = false, '<S-Tab>', '<<' }
map { 'v', noremap = false, '<Tab>', '  >><Esc>gv' }
map { 'v', noremap = false, '<S-Tab>', '<<<Esc>gv' }
map { 'n', noremap = false, '<leader>hi', ':set hlsearch! hlsearch?<CR>' }
map { 'n', noremap = false, silent = true, '<leader>hh', ':noh<CR>' }
map { 'n', noremap = false, '<leader>sc', ':%s///gc<Left><Left><Left><Left>' }
map { 'n', noremap = false, '<leader>ss', ':%s///g<Left><Left><Left>' }
map { 'v', noremap = false, '<leader>sc', 'y:%s/<C-R>"//gc<Left><Left><Left><C-R>"' }
map { 'v', noremap = false, '<leader>ss', 'y:%s/<C-R>"//g<Left><Left><C-R>"' }
map { 'n', noremap = false, 'n', 'nzzzv' }
map { 'n', noremap = false, 'N', 'Nzzzv' }
map { 'n', 's', '' }
map { 'v', 's', '' }
map { 'v', noremap = false, 's`', 'c`<C-R>"`<Esc>' }
map { 'v', noremap = false, 's\'', 'c\'<C-R>"\'<Esc>' }
map { 'v', noremap = false, 's"', 'c"<C-R>""<Esc>' }
map { 'v', noremap = false, 's[', 'c[<C-R>"]<Esc>' }
map { 'v', noremap = false, 's]', 'c[<C-R>"]<Esc>' }
map { 'v', noremap = false, 's{', 'c{<C-R>"}<Esc>' }
map { 'v', noremap = false, 's}', 'c{<C-R>"}<Esc>' }
map { 'v', noremap = false, 's(', 'c(<C-R>")<Esc>' }
map { 'v', noremap = false, 's)', 'c(<C-R>")<Esc>' }
map { 'v', noremap = false, 's<', 'c<<C-R>"><Esc>' }
map { 'v', noremap = false, 's>', 'c<<C-R>"><Esc>' }
map { 'c', noremap = false, 'w!!', 'execute \'silent! write !sudo tee % >/dev/null\' <bar> edit!<CR>' }
map { 'n', noremap = false, silent = true, '<C-f>', '/<C-R>=escape(expand("<cWORD>"), "/")<CR><CR>' }
map { 'v', noremap = false, silent = true, '<C-f>', 'y0/<C-r>"<CR>' }
map { 'n', noremap = false, silent = true, '<leader>he', ':h <C-R>=escape(expand("<cWORD>"), "/")<CR><CR>' }
map { 'v', noremap = false, silent = true, '<leader>he', 'y:h <C-r>"<CR>' }
map { 'n', noremap = false, silent = true, '<leader>ff', '<cmd>lua require("telescope.builtin").find_files({ cwd = vim.fn.expand("%:p:h") })<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fb', ':Telescope current_buffer_fuzzy_find<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fq', ':Telescope registers<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fc', ':Telescope command_history<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fh', ':Telescope help_tags<CR>' }
map { 'n', noremap = false, silent = true, '<F5>', ':lua require(\'utils.autoreload\').reload()<CR>' }
map { 'n', noremap = false, silent = true, '<leader>ee', ':NvimTreeRefresh<CR>:NvimTreeToggle<CR>' }
map { 'n', noremap = false, silent = true, '<leader>a', ':Alpha<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fn', '<cmd>lua require("ezbookmarks").AddBookmark()<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fd', '<cmd>lua require("ezbookmarks").AddBookmarkDirectory()<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fe', '<cmd>lua require("ezbookmarks").OpenBookmark()<CR>' }
map { 'n', noremap = false, silent = true, '<leader>fr', '<cmd>lua require("ezbookmarks").RemoveBookmark()<CR>' }
map { 'n', noremap = false, '<C-_>', ':exe v:count1 . "ToggleTerm"<CR>' }
map { 'n', noremap = false, silent = true, '<leader>o', ':!cd %:h && $TERMINAL &<CR>' }
map { 'v', noremap = false, silent = true, 'J', ':m \'>+1<CR>gv=gv' }
map { 'v', noremap = false, silent = true, 'K', ':m \'<-2<CR>gv=gv' }
map { 'n', noremap = false, silent = true, 'J', ':m .+1<CR>==' }
map { 'n', noremap = false, silent = true, 'K', ':m .-2<CR>==' }
map { 'n', noremap = false, silent = true, '<A-h>', '<cmd>BufferLineCyclePrev<CR>'}
map { 'n', noremap = false, silent = true, '<A-l>', '<cmd>BufferLineCycleNext<CR>'}
map { 'n', noremap = false, silent = true, '<F6>', '<cmd>lua require("run-godot").RunMainScene()<CR>' }
