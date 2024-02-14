local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	"airblade/vim-gitgutter",
	"kien/ctrlp.vim",
	"preservim/tagbar",
	{ "neoclide/coc.nvim", branch = "release" },
	"rebelot/kanagawa.nvim",
	{"nvim-treesitter/nvim-treesitter", build = ":TSUpdate"},
})

local vimrc = vim.fn.stdpath("config") .. "/vimrc.vim"
vim.cmd.source(vimrc)
