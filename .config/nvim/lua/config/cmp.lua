local cmp = require"cmp"

cmp.setup({
  snippet = {
    expand = function(args)
      require'luasnip'.lsp_expand(args.body)
    end,
  },
  mapping = {
    ['<Tab>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 's' }),
    ['<S-Tab>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 's' }),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<CR>'] = cmp.mapping.confirm({ behavior = cmp.ConfirmBehavior.Insert, select = false }),
  },
  formatting = {
    format = require("lspkind").cmp_format({
      with_text = true,
      menu = ({
        buffer = "",
        nvim_lsp = "曆",
        luasnip = "",
        nvim_lua = "",
      })
    }),
  },
  documentation = {
    border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
  },
  experimental = {
    ghost_text = true,
  },
  sources = cmp.config.sources{
    { name = 'nvim_lsp' },
    { name = 'nvim_lua' },
    { name = 'treesitter' },
    { name = 'luasnip' },
    { name = 'path' },
    { name = 'buffer', keyword_length = 4 },
  }
})
