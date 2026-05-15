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
  "sindrets/diffview.nvim",
  "samoshkin/vim-mergetool",
	"airblade/vim-gitgutter",
	"kien/ctrlp.vim",
	"preservim/tagbar",
	{ "neoclide/coc.nvim", branch = "release" },
	"rebelot/kanagawa.nvim",
	{"nvim-treesitter/nvim-treesitter", build = ":TSUpdate"},
	"https://gitlab.com/HiPhish/rainbow-delimiters.nvim.git",
  {
    "zbirenbaum/copilot.lua",
    cmd = 'Copilot',
    event = 'InsertEnter',
    config = true,
  },
})

local vimrc = vim.fn.stdpath("config") .. "/vimrc.vim"
vim.cmd.source(vimrc)

vim.diagnostic.config({
  -- Use the default configuration
  virtual_lines = true

  -- Alternatively, customize specific options
  -- virtual_lines = {
  --  -- Only show virtual line diagnostics for the current cursor line
  --  current_line = true,
  -- },
})

