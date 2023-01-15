vim.api.nvim_set_hl(0, "IndentBlanklineChar", { fg = '#3C4050' })
vim.api.nvim_set_hl(0, "IndentBlanklineContextChar", { fg = '#51566B' })

require("indent_blankline").setup {
    buftype_exclude = { "terminal" },
    filetype_exclude = { "help", "haskell", "NvimTree", "" },
    show_current_context = true,
    context_patterns = {
        "class", "return", "function", "method", "^foreach", "enum", "^if", "^while", "jsx_element", "^for", "^object", "^table", "block", "arguments", "if_statement",
        "else_clause", "jsx_element", "jsx_self_closing_element", "try_statement", "catch_clause", "import_statement", "operation_type"
    }
}
