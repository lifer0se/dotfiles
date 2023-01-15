require('nvim-treesitter.configs').setup {
    ensure_installed = { "c", "help", "lua" },
    sync_install = false,
    auto_install = true,
    highlight = {
        enable = true,
    },
}
