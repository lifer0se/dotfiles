local npairs = require("nvim-autopairs")
local Rule = require('nvim-autopairs.rule')

npairs.setup({
  check_ts = true,
  disable_in_macro = true,
  enable_moveright = false,
})

npairs.add_rule(Rule("<",">"))
