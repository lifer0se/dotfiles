vim.cmd [[packadd packer.nvim]]

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'

    use { 'kyazdani42/nvim-web-devicons' }

    use {
        'drewtempelmeyer/palenight.vim',
        config = function()
            vim.cmd('colorscheme palenight')
        end
    }

    use {
        "windwp/nvim-autopairs",
        config = function() require("nvim-autopairs").setup {} end
    }

    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.4',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    use { 'nvim-telescope/telescope-fzy-native.nvim' }

    use { 'nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' } }
    use { 'nvim-treesitter/nvim-treesitter-textobjects' }
    use { 'nvim-treesitter/nvim-treesitter-refactor' }

    use {
        'folke/noice.nvim',
		requires = {
			"MunifTanjim/nui.nvim",
			"rcarriga/nvim-notify",
		}
    }

	use {'neovim/nvim-lspconfig'}
	use {'williamboman/mason.nvim'}
	use {'williamboman/mason-lspconfig.nvim'}

	use {'hrsh7th/nvim-cmp'}
	use {'hrsh7th/cmp-buffer'}
	use {'hrsh7th/cmp-path'}
	use {'saadparwaiz1/cmp_luasnip'}
	use {'hrsh7th/cmp-nvim-lsp'}
	use {'hrsh7th/cmp-nvim-lua'}

	use {'L3MON4D3/LuaSnip'}
	use {'rafamadriz/friendly-snippets'}
    use {"ray-x/lsp_signature.nvim"}

    use { 'tpope/vim-fugitive' }
    use { 'mfussenegger/nvim-dap' }

    use { 'mbbill/undotree' }
    use { 'numToStr/Comment.nvim' }
    use { 'tpope/vim-surround' }
    use { 'kyazdani42/nvim-tree.lua' }
    use { 'norcalli/nvim-colorizer.lua' }

    use { 'hoob3rt/lualine.nvim' }
    use { 'akinsho/nvim-bufferline.lua' }

    use { 'lukas-reineke/indent-blankline.nvim' }
    use { 'kevinhwang91/nvim-fFHighlight' }
    use { 'folke/which-key.nvim' }

end)
