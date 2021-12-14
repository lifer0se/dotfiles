local alpha = require'alpha'
local startify = require'alpha.themes.startify'

local function button(sc, txt, keybind, keybind_opts, hl)
  local b = startify.button(sc, txt, keybind, keybind_opts)
  -- local hl = "Keyword"
  b.opts.hl_shortcut = { {"Identifier", 0, 1}, {"Number", 2, 3}, {"Identifier", 4, 5}, {hl, 8, 9} }
  return b
end


startify.section.header.opts.hl = "Identifier"
startify.section.header.val = {
[[                                                ]],
[[                                                ]],
[[   ________            _____  _____.__          ]],
[[   \\      \  _____  __\\   \//   /|__| ______  ]],
[[   //   |   \// __ \/  _ \   Y   /||  |//     \ ]],
[[  //    |    \  ___(  <_> )     / ||  ||  Y Y  \]],
[[  \\____|__  /\___  >____/\\___/  ||__||__|_|  /]],
[[          \\/    \\/                        \\/ ]],
}

-- startify.mru.items_number = 9

startify.opts.layout[4] = { type = "padding", val = 0 }
startify.opts.layout[8] = startify.section.top_buttons
startify.opts.layout[9] = startify.section.bottom_buttons

startify.section.mru.val[2].val = "Recent files:"
startify.section.mru.val[4] = { type = "group", val = function() return { startify.mru(1, false, 9)} end}

startify.section.mru_cwd.val = { { type = "padding", val = 0 } }

startify.section.top_buttons.val = {
    button( "e", "  New file" , ":ene <BAR> startinsert <CR>",nil, "Function"),
}


startify.section.bottom_buttons.val = {
    button( "q", "  Quit" , ":qa<CR>",nil, "Keyword"),
}

alpha.setup(startify.opts)
