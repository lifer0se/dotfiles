
local custom_onedark = require'lualine.themes.onedark'
custom_onedark.normal.b.bg = '#20222E'
custom_onedark.normal.b.fg = '#ebdbb2'
custom_onedark.normal.c.bg = '#2B2E37'
custom_onedark.normal.c.fg = '#ebdbb2'
custom_onedark.inactive.c.bg = '#2B2E37'

require'lualine'.setup {
  options = {
    icons_enabled = true,
    theme = custom_onedark,
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = { 'NvimTree', 'alpha' },
    symbols = {error = ' ', warn = ' ', info = ' ', hint = ' '}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff' },
    lualine_c = {'filename'},
    lualine_x = {{'diagnostics', sources={'nvim_lsp'}}, 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {'branch', 'diff' },
    lualine_c = {'filename'},
    lualine_x = {{'diagnostics', sources={'nvim_lsp'}}, 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  tabline = {},
  extensions = {}
}
