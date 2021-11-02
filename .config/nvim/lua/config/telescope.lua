require('telescope').setup{
  defaults = {
		prompt_prefix = '🔍  ',
		selection_caret = 'ﰲ ',
		hidden = true,
	},
	pickers = {
		find_files = {
			layout_config = {
				preview_width = 0.6,
	 		},
		},
		help_tags = {
			layout_config = {
				preview_width = 0.7,
	 		},
		},
		current_buffer_fuzzy_find = {
			sorting_strategy = 'ascending',
			layout_config = {
				preview_width = 0.5,
	 		},
		},
	},
	extensions = {
		fyz_native = {
			override_generic_order = false,
			override_files_sorter = true,
		}
	}
}

require('telescope').load_extension('fzy_native')
require('telescope').load_extension('sessions')
