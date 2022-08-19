
require'lualine'.setup {
  options = {
    icons_enabled = true,
    theme = 'palenight',
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = { 'NvimTree', 'alpha' },
    symbols = {error = ' ', warn = ' ', info = ' ', hint = ' '}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff' },
    lualine_c = {'filename'},
    lualine_x = {{'diagnostics', sources={'nvim_diagnostic'}}, 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {'branch', 'diff' },
    lualine_c = {'filename'},
    lualine_x = {{'diagnostics', sources={'nvim_diagnostic'}}, 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  tabline = {},
  extensions = {}
}
