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
map { 'n', noremap = false, '<C-s>', ':w<cr>' }
map { 'i', noremap = false, '<C-s>', '<esc>' }
map { 'n', noremap = false, silent = true, '<leader>gg', ':G<cr>' }
map { 'n', noremap = false, '<leader>gc', ':Git commit<cr>' }
map { 'n', noremap = false, '<leader>gp', ':Git push<cr>' }
map { 'n', noremap = false, '<leader>u', ':UndotreeToggle<cr>' }
map { 'n', noremap = false, 'Y', 'y$' }
map { 'n', noremap = false, '<leader>p', '"_dP' }
map { 'n', noremap = false, '<leader>P', '"_dp' }
map { 'v', noremap = false, '<leader>p', '"_dP' }
map { 'v', noremap = false, '<leader>P', '"_dp' }
map { 'n', noremap = false, '<leader>tn', ':tabnew<cr>' }
map { 'n', noremap = false, silent = true, '<C-z>', ':bd!<cr>' }
map { 'n', noremap = false, '<C-c>', '<C-w>c' }
map { 'n', noremap = false, '<C-h>', '<C-w>h' }
map { 'n', noremap = false, '<C-j>', '<C-w>j' }
map { 'n', noremap = false, '<C-k>', '<C-w>k' }
map { 'n', noremap = false, '<C-l>', '<C-w>l' }
map { 'n', noremap = true, silent = true, '<C-Up>', ':resize +2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Down>', ':resize -2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Left>', ':vertical resize +2<CR>' }
map { 'n', noremap = true, silent = true, '<C-Right>',':vertical resize -2<CR>' }
map { 'n', noremap = false, '<Tab>', '  >>' }
map { 'n', noremap = false, '<S-Tab>', '<<' }
map { 'v', noremap = false, '<Tab>', '  >><Esc>gv' }
map { 'v', noremap = false, '<S-Tab>', '<<<Esc>gv' }
map { 'n', noremap = false, '<leader>h', ':set hlsearch! hlsearch?<cr>' }
map { 'n', noremap = false, '<leader>sc', ':%s///gc<Left><Left><Left><Left>' }
map { 'n', noremap = false, '<leader>ss', ':%s///g<Left><Left><Left>' }
map { 'v', noremap = false, '<leader>sc', 'y:%s/<C-R>"//gc<Left><Left><Left><C-R>"' }
map { 'v', noremap = false, '<leader>ss', 'y:%s/<C-R>"//g<Left><Left><C-R>"' }
map { 'n', noremap = false, 'n', 'nzzzv' }
map { 'n', noremap = false, 'N', 'Nzzzv' }
map { 'v', noremap = false, 's', '' }
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
map { 'c', noremap = false, 'w!!', 'execute \'silent! write !sudo tee % >/dev/null\' <bar> edit!<cr>' }
map { 'n', noremap = false, silent = true, '<C-f>', '/<C-R>=escape(expand("<cWORD>"), "/")<cr><cr>:set hls<cr>' }
map { 'v', noremap = false, silent = true, '<C-f>', 'y0/<C-r>"<cr>:set hls<cr>' }
map { 'n', noremap = false, silent = true, '<leader>f', ':h <C-R>=escape(expand("<cWORD>"), "/")<cr><cr>' }
map { 'v', noremap = false, silent = true, '<leader>f', 'y:h <C-r>"<cr>' }
map { 'n', noremap = false, silent = true, '<leader>t', ':vsplit <C-R>=expand("%:p:h") . "/" <CR>list.todo.md<CR><C-w>30<' }
map { 'v', noremap = false, silent = true, 'J', ':m \'>+1<cr>gv=gv' }
map { 'v', noremap = false, silent = true, 'K', ':m \'<-2<cr>gv=gv' }
map { 'n', noremap = false, silent = true, 'J', ':m .+1<cr>==' }
map { 'n', noremap = false, silent = true, 'K', ':m .-2<cr>==' }
map { 'n', noremap = false, silent = true, '`', ':lua require("harpoon.mark").add_file()<cr>' }
map { 'n', noremap = false, silent = true, '<A-`>', ':lua require("harpoon.ui").toggle_quick_menu()<cr>' }
map { 'n', noremap = false, silent = true, '<A-1>', ':lua require("harpoon.ui").nav_file(1)<cr>' }
map { 'n', noremap = false, silent = true, '<A-2>', ':lua require("harpoon.ui").nav_file(2)<cr>' }
map { 'n', noremap = false, silent = true, '<A-3>', ':lua require("harpoon.ui").nav_file(3)<cr>' }
map { 'n', noremap = false, silent = true, '<A-4>', ':lua require("harpoon.ui").nav_file(4)<cr>' }
map { 'n', noremap = false, silent = true, 'H', '<cmd>BufferLineCyclePrev<cr>'}
map { 'n', noremap = false, silent = true, 'L', '<cmd>BufferLineCycleNext<cr>'}
map { 'n', noremap = false, silent = true, '<leader>ff', ':Telescope find_files<cr>' }
map { 'n', noremap = false, silent = true, '<leader>fb', '<cmd>lua require(\'telescope\').extensions.vinegar.file_browser()<cr>' }
map { 'n', noremap = false, silent = true, '<leader>fc', ':Telescope find_files cwd=~/.config<cr>' }
map { 'n', noremap = false, silent = true, '<leader>fv', ':Telescope find_files cwd=~/.config/nvim<cr>' }
map { 'n', noremap = false, silent = true, '<leader>fg', ':Telescope live_grep<cr>' }
map { 'n', noremap = false, silent = true, '<leader>fr', ':Telescope registers<cr>' }
map { 'n', noremap = false, silent = true, '<C-_>', ':Telescope current_buffer_fuzzy_find<cr>' }
map { 'n', noremap = false, silent = true, '<F5>', ':lua require(\'utils.autoreload\').reload()<cr>' }
map { 'n', noremap = false, '<leader>e', ':NvimTreeFindFile<cr>' }