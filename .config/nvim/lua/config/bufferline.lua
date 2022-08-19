local offsets = {
    {
        filetype = "NvimTree",
        text = "NvimTree",
        highlight = "BufferLineFill",
        text_align = "left",
    },
}
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
    modified_icon = "",
		show_tab_indicators = true,
		show_close_icon = false,
		show_buffer_close_icons = false,
    left_trunc_marker = 'ﰯ',
    right_trunc_marker = 'ﰲ',
    separator_style = "none",
    offsets = offsets,
    custom_filter = function(buf)
      if (vim.bo[buf].filetype == "help") then
        return false
      end

      local tab_num = 0
      for _ in pairs(vim.api.nvim_list_tabpages()) do tab_num = tab_num + 1 end
      if tab_num > 1 then
          return false
      else
          return true
      end
    end,
	},
	highlights = {
		buffer_selected =
        {
			gui = "bold"
		},
        modified =
        {
			gui = "bold"
		},
		modified_visible =
        {
			gui = "bold"
		},
		modified_selected =
        {
			gui = "bold"
		},
		duplicate_selected =
        {
			gui = "italic",
		},
		duplicate_visible =
        {
			gui = "italic",
		},
		duplicate =
        {
			gui = "italic",
		},
	};
}
require'bufferline'.sort_buffers_by('directory')
