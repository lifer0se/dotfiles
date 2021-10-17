local Plug = vim.fn['plug#']

vim.call('plug#begin', '~/.local/share/nvim/plugged')

Plug('mhinz/vim-startify')
Plug('mbbill/undotree')
Plug('preservim/nerdcommenter')
Plug('tpope/vim-surround')
Plug('aserebryakov/vim-todo-lists')
Plug('tmsvg/pear-tree')
Plug('kyazdani42/nvim-tree.lua')

Plug('tpope/vim-fugitive')
Plug('tpope/vim-rhubarb')

Plug('kyazdani42/nvim-web-devicons')
Plug('morhetz/gruvbox')
Plug('norcalli/nvim-colorizer.lua')

Plug('nvim-lua/popup.nvim')
Plug('nvim-lua/plenary.nvim')

Plug('neovim/nvim-lspconfig')
Plug('ray-x/lsp_signature.nvim')
Plug('hrsh7th/nvim-cmp')
Plug('hrsh7th/cmp-nvim-lsp')
Plug('hrsh7th/cmp-nvim-lua')
Plug('hrsh7th/cmp-buffer')
Plug('hrsh7th/cmp-path')
Plug('onsails/lspkind-nvim')
Plug('ThePrimeagen/harpoon')

Plug('nvim-treesitter/nvim-treesitter', {['do'] = vim.fn[':TSUpdate']})
Plug('nvim-treesitter/playground')

Plug('nvim-telescope/telescope.nvim')
Plug('nvim-telescope/telescope-fzy-native.nvim')
Plug('rudism/telescope-vinegar.nvim')

Plug('hoob3rt/lualine.nvim')
Plug('akinsho/bufferline.nvim')

vim.call('plug#end')
