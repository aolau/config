set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tpope/vim-fugitive'
Plugin 'airblade/vim-gitgutter'

call vundle#end()

filetype plugin indent on
colorscheme apprentice
set tabstop=4 shiftwidth=4 expandtab

set nofoldenable

map <Space>mm :make<CR>
map <Space>mc :make clean<CR>
map <Space>mn :cnext <CR>
map <Space>mp :cprevious<CR>

map <Space>gs :Gstatus<CR>

map <Space>gg :GitGutterToggle<CR>

