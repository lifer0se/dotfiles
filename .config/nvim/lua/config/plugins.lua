vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)

  use { 'wbthomason/packer.nvim' }
  use { 'goolord/alpha-nvim' }
  use { 'mbbill/undotree' }
  use { 'winston0410/commented.nvim' }
  use { 'tpope/vim-surround' }
  use { 'kyazdani42/nvim-tree.lua' }
  use { 'akinsho/toggleterm.nvim' }
  use { 'tpope/vim-fugitive' }
  use { 'kyazdani42/nvim-web-devicons' }
  use { 'morhetz/gruvbox' }
  use { 'norcalli/nvim-colorizer.lua' }
  use { 'nvim-lua/popup.nvim' }
  use { 'nvim-lua/plenary.nvim' }
  use { 'neovim/nvim-lspconfig' }
  use { 'williamboman/nvim-lsp-installer' }
  use { 'hrsh7th/nvim-cmp' }
  use { 'hrsh7th/cmp-nvim-lsp' }
  use { 'hrsh7th/cmp-nvim-lua' }
  use { 'hrsh7th/cmp-cmdline' }
  use { 'hrsh7th/cmp-buffer' }
  use { 'hrsh7th/cmp-path' }
  use { 'hrsh7th/cmp-vsnip' }
  use { 'hrsh7th/vim-vsnip' }
  use { 'rafamadriz/friendly-snippets' }
  use { 'onsails/lspkind-nvim' }
  use { 'ray-x/lsp_signature.nvim' }
  use { 'nvim-treesitter/nvim-treesitter' }
  use { 'nvim-treesitter/nvim-treesitter-textobjects' }
  -- use { 'nvim-treesitter/playground' }
  use { 'nvim-telescope/telescope.nvim' }
  use { 'nvim-telescope/telescope-fzy-native.nvim' }
  use { 'hoob3rt/lualine.nvim' }
  use { 'akinsho/bufferline.nvim' }
  use { 'tmsvg/pear-tree' }
  use { 'lukas-reineke/indent-blankline.nvim' }
  use { 'j-hui/fidget.nvim' }

  -- use { 'lifer0se/ezbookmarks.nvim' }

end)
