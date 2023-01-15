vim.keymap.set('n', '<leader>ee', ':NvimTreeFindFileToggle<CR>')
vim.g.nvim_tree_indent_markers = 1
require'nvim-tree'.setup {
    hijack_cursor = true,
    view = {
        adaptive_size = true,
    },
    renderer = {
        group_empty = true,
    },
}
