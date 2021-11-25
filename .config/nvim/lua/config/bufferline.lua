require'bufferline'.setup{
	options = {
    diagnostics = "nvim_lsp",
    diagnostics_indicator = function(count, level, diagnostics_dict, context)
      for e, n in pairs(diagnostics_dict) do
        if e == "error" or e == "warning" then
          return ""
        else
          return ""
        end
      end
    end,
		middle_mouse_command = 'bdelete! %d',
    modified_icon = "",
		show_tab_indicators = false,
		show_close_icon = false,
		show_buffer_close_icons = false,
    left_trunc_marker = 'ﰯ',
    right_trunc_marker = 'ﰲ',
    offsets = {{filetype = "NvimTree"}}
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
			guifg = '#716C5F',
			gui = "bold"
		},
		modified_visible = {
			guibg = '#2B2E37',
			guifg = '#716C5F',
			gui = "bold"
		},
		modified_selected = {
			guibg = '#2B2E37',
			guifg = '#AFD787',
			gui = "bold"
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
		error_diagnostic = {
			guibg = '#2B2E37',
			guifg = '#cc241d',
      gui = 'bold'
		},
		error_diagnostic_visible = {
			guibg = '#2B2E37',
			guifg = '#cc241d',
      gui = 'bold'
		},
    error_diagnostic_selected= {
			guibg = '#2B2E37',
			guifg = '#cc241d',
      gui = 'bold'
		},
		error = {
			guibg = '#2B2E37',
			guifg = '#AFA58A'
		},
		error_visible = {
			guibg = '#2B2E37',
			guifg = '#AFA58A'
		},
    error_selected = {
			guibg = '#2B2E37',
			guifg = '#AFD787',
      gui = 'bold'
		},
		warning_diagnostic = {
			guibg = '#2B2E37',
			guifg = '#FABD2F',
      gui = 'bold'
		},
		warning_diagnostic_visible = {
			guibg = '#2B2E37',
			guifg = '#FABD2F',
      gui = 'bold'
		},
    warning_diagnostic_selected= {
			guibg = '#2B2E37',
			guifg = '#FABD2F',
      gui = 'bold'
		},
		warning = {
			guibg = '#2B2E37',
			guifg = '#AFA58A'
		},
		warning_visible = {
			guibg = '#2B2E37',
			guifg = '#AFA58A'
		},
    warning_selected = {
			guibg = '#2B2E37',
			guifg = '#AFD787',
      gui = 'bold'
		},
		info = {
			guibg = '#2B2E37',
			guifg = '#688D6A',
		},
		diagnostic = {
			guibg = '#2B2E37',
			guifg = '#688D6A',
		},
		diagnostic_visible = {
			guibg = '#2B2E37',
			guifg = '#688D6A',
		},
    diagnostic_selected = {
			guibg = '#2B2E37',
			guifg = '#688D6A',
		},
	};
}
