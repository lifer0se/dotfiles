
require('toggleterm').setup {
  size = 40,
	open_mapping = [[<c-_>]],
  hide_numbers = true,
  shade_filetypes = {},
  shade_terminals = false,
  start_in_insert = true,
  insert_mappings = true, -- whether or not the open mapping applies in insert mode
  persist_size = true,
  direction = 'float',
  close_on_exit = true, -- close the terminal window when the process exits
  shell = vim.o.shell, -- change the default shell
  float_opts = {
    border = 'curved',
    width = 150,
    height = 30,
    winblend = 0,
    highlights = {
      border = "Normal",
      background = "Normal",
    }
  }
}
