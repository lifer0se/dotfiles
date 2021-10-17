require'bufferline'.setup{
	options = {
		middle_mouse_command = 'bdelete! %d',
		show_tab_indicators = false,
		show_close_icon = false,
		show_buffer_close_icons = false,
	},
	highlights = {
		fill = {
			guifg = '#AFA58A',
			guibg = '#444444'
		},
		background = {
			guifg = '#AFA58A',
			guibg = '#444444'
		},
		buffer_visible = {
			guifg = '#AFA58A',
			guibg = '#444444'
		},
		buffer_selected = {
			guibg = '#3A3A3A',
			guifg = '#AFD787',
			gui = "bold"
		},
		modified = {
			guibg = '#444444',
			guifg = '#716C5F'
		},
		modified_visible = {
			guibg = '#444444',
			guifg = '#716C5F'
		},
		modified_selected = {
			guibg = '#3A3A3A',
			guifg = '#AFD787',
		},
		duplicate_selected = {
			guibg = '#3A3A3A',
			guifg = '#83A165',
			gui = "italic",
		},
		duplicate_visible = {
			guifg = '#827B67',
			guibg = '#444444',
			gui = "italic",
		},
		duplicate = {
			guifg = '#827B67',
			guibg = '#444444',
			gui = "italic",
		},
		close_button = {
			guibg = '#444444',
			guifg = '#444444',
		},
		close_button_visible = {
			guibg = '#444444',
		},
		close_button_selected = {
			guibg = '#3A3A3A',
		},
		separator_selected = {
			guibg = '#3A3A3A',
			guifg = '#3A3A3A'
		},
		separator_visible = {
			guibg = '#444444',
			guifg = '#444444'
		},
		separator = {
			guibg = '#444444',
			guifg = '#444444'
		},
		indicator_selected = {
			guifg = '#3A3A3A',
			guibg = '#3A3A3A'
		},
	};
}
