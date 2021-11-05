require'bufferline'.setup{
	options = {
		middle_mouse_command = 'bdelete! %d',
    modified_icon = "",
		show_tab_indicators = false,
		show_close_icon = false,
		show_buffer_close_icons = false,
	},
	highlights = {
		fill = {
			guifg = '#AFA58A',
			guibg = '#2B2E37'
		},
		background = {
			guifg = '#AFA58A',
			guibg = '#2B2E37'
		},
		buffer_visible = {
			guifg = '#AFA58A',
			guibg = '#2B2E37'
		},
		buffer_selected = {
			guibg = '#2B2E37',
			guifg = '#AFD787',
			gui = "bold"
		},
		modified = {
			guibg = '#2B2E37',
			guifg = '#716C5F'
		},
		modified_visible = {
			guibg = '#2B2E37',
			guifg = '#716C5F'
		},
		modified_selected = {
			guibg = '#2B2E37',
			guifg = '#AFD787',
		},
		duplicate_selected = {
			guibg = '#2B2E37',
			guifg = '#83A165',
			gui = "italic",
		},
		duplicate_visible = {
			guifg = '#827B67',
			guibg = '#2B2E37',
			gui = "italic",
		},
		duplicate = {
			guifg = '#827B67',
			guibg = '#2B2E37',
			gui = "italic",
		},
		close_button = {
			guibg = '#2B2E37',
			guifg = '#2B2E37',
		},
		close_button_visible = {
			guibg = '#2B2E37',
		},
		close_button_selected = {
			guibg = '#2B2E37',
		},
		separator_selected = {
			guibg = '#2B2E37',
			guifg = '#2B2E37'
		},
		separator_visible = {
			guibg = '#2B2E37',
			guifg = '#2B2E37'
		},
		separator = {
			guibg = '#2B2E37',
			guifg = '#2B2E37'
		},
		indicator_selected = {
			guifg = '#2B2E37',
			guibg = '#2B2E37'
		},
	};
}
