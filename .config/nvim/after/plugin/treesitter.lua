require('nvim-treesitter.configs').setup {
    ensure_installed = { "c_sharp", "lua" },
    sync_install = false,
    auto_install = true,
    highlight = {
        enable = true,
    },
    indent = {
        enable = true
    },
}
