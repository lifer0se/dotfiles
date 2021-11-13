---@diagnostic disable: different-requires
vim.cmd [[packadd packer.nvim]]

vim.opt.runtimepath:append("~/development/ezbookmarks.nvim")
require('ezbookmarks').setup{
  cwd_on_open = 1,
  use_bookmark_dir = 1,
  open_new_tab = 0,
}

return require('packer').startup(function(use)

  use { 'wbthomason/packer.nvim' }

  use { 'goolord/alpha-nvim',
    config = require('config.alpha')
  }

  use { 'mbbill/undotree' }

  use { 'winston0410/commented.nvim',
    config = require('commented').setup{
      keybindings = {n = "<leader>cc", v = "<leader>cc", nl = "<leader>cc"}
    }
  }
  use { 'tpope/vim-surround' }

  use { 'kyazdani42/nvim-tree.lua',
    config = require('config.nvim-tree')
  }

  use { 'akinsho/toggleterm.nvim',
    config = require('config.toggleterm')
  }

  use { 'tpope/vim-fugitive' }
  use { 'tpope/vim-rhubarb' }

  use { 'kyazdani42/nvim-web-devicons' }
  use { 'morhetz/gruvbox' }
  use { 'norcalli/nvim-colorizer.lua',
    config = require('colorizer').setup()
  }
  use { 'nvim-lua/popup.nvim' }
  use { 'nvim-lua/plenary.nvim' }

  use { 'williamboman/nvim-lsp-installer' }
  use { 'neovim/nvim-lspconfig',
    config = require('config.lsp')
  }

  use { 'hrsh7th/nvim-cmp',
    config = require('config.cmp')
  }
  use { 'hrsh7th/cmp-nvim-lsp' }
  use { 'hrsh7th/cmp-nvim-lua' }
  use { 'hrsh7th/cmp-cmdline' }
  use { 'hrsh7th/cmp-buffer' }
  use { 'hrsh7th/cmp-path' }
  use { 'saadparwaiz1/cmp_luasnip' }
  use { 'L3MON4D3/LuaSnip' }

  use { 'ray-x/cmp-treesitter',
    config = require('config.treesitter')
  }

  use { 'onsails/lspkind-nvim',
    config = require('config.lspkind')
  }
  use { 'ray-x/lsp_signature.nvim',
    config =require "lsp_signature".setup({
      bind = true,
      doc_lines = 0,
      hint_enable = false,
      handler_opts = {
        border = "single",
      },
    })
  }

  use { 'nvim-treesitter/nvim-treesitter' }
  use { 'nvim-treesitter/nvim-treesitter-textobjects' }
  use { 'nvim-treesitter/playground' }

  use { 'nvim-telescope/telescope.nvim',
    config = require('config.telescope')
  }
  use { 'nvim-telescope/telescope-fzy-native.nvim' }
  use { 'hoob3rt/lualine.nvim',
    config = require('config.lualine')
  }
  use { 'akinsho/bufferline.nvim',
    config = require('config.bufferline')
  }
  use { 'jghauser/mkdir.nvim',
    config = require('mkdir')
  }
  use { 'cohama/lexima.vim' }

  use { 'luukvbaal/stabilize.nvim',
    config = require("stabilize").setup()
  }
  use { 'lukas-reineke/indent-blankline.nvim',
    config = require("indent_blankline").setup {
      show_current_context = true,
      buftype_exclude = {"terminal"},
      filetype_exclude = {"alpha", "help", "haskell", ""}
    }
  }

  -- use { 'lifer0se/ezbookmarks.nvim' }

end)
