set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'Xuyuanp/nerdtree-git-plugin'
Plugin 'tpope/vim-fugitive'
" Plugin 'airblade/vim-gitgutter'
Plugin 'vim-scripts/Conque-GDB'
Plugin 'vim-airline/vim-airline'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'kovisoft/slimv'
Plugin 'majutsushi/tagbar'

call vundle#end()

filetype plugin indent on

colorscheme apprentice

set tabstop=4 shiftwidth=4 expandtab

set ignorecase
set smartcase
set hlsearch
set incsearch
set number
set nofoldenable

set nobackup
set noswapfile

set cinoptions+=g0

set completeopt-=preview

set laststatus=2
let g:airline#extensions#whitespace#enabled = 0

inoremap <C-e> <Esc>

map <Space>q :q<CR>
map <Space>w :w<CR>
map <Space>s :% s/
map <Space>h :nohlsearch<CR>
map <Space>r :set relativenumber nonumber<CR>
map <Space>a :set norelativenumber number<CR>

map <Space>tt :NERDTreeToggle<CR>
map <Space>tg :TagbarToggle<CR>
map <Space>b :CtrlPBuffer<CR>
map <Space>f :CtrlP<CR>

map <Space>mm :make<CR><CR>
map <Space>mc :make clean<CR><CR>
map <space>n :cnext <cr>
map <space>p :cprevious<cr>
map <space>cf :cfirst<cr>
map <space>cl :clast<cr>
map <Space>co :copen<CR>
map <Space>cc :cclose<CR>

map <Space>gs :Gstatus<CR>
map <Space>ge :Gedit<CR>
map <Space>gb :Gblame<CR>
map <Space>gl :Glog -10<CR><CR><CR> :copen <CR>
map <Space>gla :Glog --<CR><CR><CR> :copen <CR>
map <Space>gq :cclose<CR> :Gedit <CR>
map <Space>gg :GitGutterToggle<CR>

