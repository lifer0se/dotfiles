
local custom_onedark = require'lualine.themes.onedark'
custom_onedark.normal.b.bg = '#20222E'
custom_onedark.normal.b.fg = '#ebdbb2'
custom_onedark.normal.c.bg = '#2B2E37'
custom_onedark.normal.c.fg = '#ebdbb2'

require'lualine'.setup {
  options = {
    icons_enabled = true,
    theme = custom_onedark,
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = {}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff',
      symbols = {added = '+', modified = '~', removed = '-'}
	},
    lualine_c = {''},
    lualine_x = {'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  extensions = {}
}
