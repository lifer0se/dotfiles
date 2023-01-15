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
        'nvim-telescope/telescope.nvim', tag = '0.1.0',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    use { 'nvim-telescope/telescope-fzy-native.nvim' }

    use('nvim-treesitter/nvim-treesitter', { run = ':TSUpdate' })
    use { 'nvim-treesitter/nvim-treesitter-textobjects' }

    use {'Mofiqul/trld.nvim'}
    use {
        'VonHeikemen/lsp-zero.nvim',
        requires = {
            -- LSP Support
            {'neovim/nvim-lspconfig'},
            {'williamboman/mason.nvim'},
            {'williamboman/mason-lspconfig.nvim'},

            -- Autocompletion
            {'hrsh7th/nvim-cmp'},
            {'hrsh7th/cmp-buffer'},
            {'hrsh7th/cmp-path'},
            {'saadparwaiz1/cmp_luasnip'},
            {'hrsh7th/cmp-nvim-lsp'},
            {'hrsh7th/cmp-nvim-lua'},

            -- Snippets
            {'L3MON4D3/LuaSnip'},
            {'rafamadriz/friendly-snippets'},
        }
    }

    use { 'tpope/vim-fugitive' }

    use { 'mbbill/undotree' }
    use { 'numToStr/Comment.nvim' }
    use { 'tpope/vim-surround' }
    use { 'kyazdani42/nvim-tree.lua' }
    use { 'norcalli/nvim-colorizer.lua' }

    use { 'hoob3rt/lualine.nvim' }
    use { 'akinsho/nvim-bufferline.lua' }

    use { 'lukas-reineke/indent-blankline.nvim' }
    use { 'j-hui/fidget.nvim' }
    use { 'kevinhwang91/nvim-fFHighlight' }
    use { 'folke/which-key.nvim' }

end)
