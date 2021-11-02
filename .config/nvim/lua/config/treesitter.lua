require'nvim-treesitter.configs'.setup {
  ensure_installed = {"lua", "haskell"},
  highlight = {
    enable = true,
  },
  cts = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ["<leader>s"] = "@parameter.inner",
      },
      swap_previous = {
        ["<leader>S"] = "@parameter.inner",
      },
    },
    context_commentstring = {
      enable = true,
      enable_autocmd = true,
    },
  }
}
