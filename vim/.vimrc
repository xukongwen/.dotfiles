"====================插件安装====================

call plug#begin('~/.vim/plugged')
	Plug 'junegunn/goyo.vim'  " 文字居中
	Plug 'tpope/vim-markdown' " markdown
	Plug 'morhetz/gruvbox'    " theme
	Plug 'vim-scripts/fountain.vim'  " 剧本模式
	Plug 'airblade/vim-gitgutter'    " git修改提示
	Plug 'vim-airline/vim-airline'   " 提示bar
	"Plug 'preservim/nerdtree'        " 文件树
	Plug 'mhinz/vim-startify'        " 开始菜单
	Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
	Plug 'junegunn/fzf.vim'
	Plug 'yuttie/comfortable-motion.vim'
"	Plug 'vimwiki/vimwiki'
	Plug 'francoiscabrol/ranger.vim'
	Plug 'yianwillis/vimcdoc'
	Plug 'nightsense/carbonized'
call plug#end()

"=====================居中goyo插件设置==============

function! s:goyo_enter()
	let b:quitting = 0
	let b:quitting_bang = 0
	let g:goyo_height = 100
	autocmd QuitPre <buffer> let b:quitting = 1
	cabbrev <buffer> q! let b:quitting_bang = 1 <bar> q!
endfunction

function! s:goyo_leave()
	if b:quitting && len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) ==1
		if b:quitting_bang
			qa!
		else 
			qa
		endif
	endif
endfunction

autocmd! VimEnter * :Goyo
autocmd! User GoyoEnter call <SID>goyo_enter()
autocmd! User GoyoLeave call <SID>goyo_leave()


"============基础设置=============================
"set cursorline 
" 几乎无论是插入还是不同模式，都使文本居中，可根据自己喜好开关
" 同时退出插入模式时自动保存
autocmd VimEnter * set scrolloff=999
autocmd InsertEnter * set showmode | set scrolloff=1 | norm! zz
autocmd InsertLeave * set noshowmode | set scrolloff=999 | :w

" 再次进入Vim直接打开上次退出时的文档,离开的时候未来写自动同步，git和坚果云
"autocmd VimLeave * :mksession! ~/.last.vim
"autocmd VimEnter * :so ~/.last.vim

let g:ranger_map_keys = 0
let g:NERDTreeHijackNetrw = 0
let g:ranger_replace_netrw = 1

set nocompatible
filetype plugin on
syntax on
set encoding=UTF-8
set autoread "文件被外部修改后，自动重新读取
set autowrite "自动保存
set t_Co=256
set noswapfile 
set noshowmode
set wrap
set wildmenu
set background=dark 
"set number
colorscheme gruvbox
let g:auto_save = 1
"=====================Leader Map=======================
" leader设置为是空格键
let mapleader = " "

" , s 是储存文件
noremap <leader>s :w<cr> 
" 空格m，返回到主菜单
noremap <leader>m :Startify<cr>
" 空格f，超级查找
noremap <leader>f :Files<cr>
" 空格q，直接退出（从主页）
noremap <leader>q :qa<cr>
" 空格r，进入文件管理（暂时不能用）
noremap <leader>r :Ranger<cr>
" 空格b，进入buffer
noremap <leader>b :Buffer<cr>
" 空格h，进入帮助
noremap <leader>h :e ~/none-os/none-os-help.md<cr>
" 空格t，打开一个新tab
noremap <leader>t :tabe<CR>
" 空格v，分屏
noremap <leader>v :vnew<CR>
" 空格e，打開文件瀏覽
noremap <leader>e :E<cr>
" 空格n，新建文檔
noremap <leader>n :edit<cr>

"======================按键映射========================================

"map <C-n> :NERDTree<CR>
"map <C-t> :NERDTreeToggle<CR>

" 为了中文写作模式将j k改为实际行的上下操作
nnoremap k gk
nnoremap gk k
nnoremap j gj
nnoremap gj j
" 全选
map <silent> <C-A> gg v G
" 将退出模式由ESC改为连按两次\
inoremap \\ <ESC>

"===============自定义命令=========================

command! Reload execute "source ~/.vimrc"
command! Config execute ":e $MYVIMRC"

"========================下面是st的一些设置==============================

let g:startify_session_dir = '~/.vim/session'
" 实时更新文件
let g:startify_update_oldfiles = 1
" 总是在项目的根目录，这样有利于搜索
let g:startify_change_to_vcs_root = 1

" 自定义页头
let g:startify_custom_header =
            \ startify#fortune#cowsay('', '═','║','╔','╗','╝','╚')

" 自定义页尾
let g:startify_custom_footer =
           \ ['', "   Nono-OS 超早期测试版0.004c", '']

" 自定义开头画面

let g:ascii = [
          \ '        							     		',
	  \ '  __   __     ______     __   __     ______     ______     ______		',    
	  \ ' /\ "-.\ \   /\  __ \   /\ "-.\ \   /\  ___\   /\  __ \   /\  ___\		',   
	  \ ' \ \ \-.\ \  \ \ \/\ \  \ \ \-.  \  \ \  __\   \ \ \/\ \  \ \___  \	',  
 	  \ '  \ \_\\"\ \  \ \_____\  \ \_\\"\_\  \ \_____\  \ \_____\  \/\_____\	', 
  	  \ '   \/_/ \/_/   \/_____/   \/_/ \/_/   \/_____/   \/_____/   \/_____/	',
	  \ '										',
	  \ '   任何时候需要帮助请按：“空格+h”										',
          \]

let g:startify_custom_header = g:ascii
" 设置的命令
function s:setting()
    return [
          \ { 'line': 'VIM设置', 'cmd': 'e ~/.vimrc' },
          \ { 'line': 'ZSH设置', 'cmd': 'e ~/.zshrc' },
          \ { 'line': '字体设置', 'cmd': 'e ~/.fbtermrc' },
          \ ]
endfunction
" 帮助的命令
function s:helping()
    return [
          \ { 'line': 'VIM帮助', 'cmd': 'e ~/none-os/vim-help.md' },
          \ { 'line': 'Linux帮助', 'cmd': 'e ~/none-os/linux-help.md' },
          \ ]
endfunction

function s:about()
    return [
          \ { 'line': '关于None-OS', 'cmd': 'e ~/none-os/about.md' },
          \ ]
endfunction

" 常用命令
function s:os()
    return [
          \ { 'line': '浏览文件', 'cmd': 'E' },
          \ { 'line': '应用VIM修改', 'cmd': 'Reload' },
          \ { 'line': '安装插件', 'cmd': 'PlugInstall' },
          \ { 'line': '退出', 'cmd': 'qa' },
          \ ]
endfunction
" 自定义开头欢迎语句
"let g:startify_custom_header_quotes =
"      \ startify#fortune#predefined_quotes() + [['None-OS 0.004a', '是吗']]

" 菜单布局
let g:startify_lists = [
	  \ { 'header': ['  常用命令'],        'type': function('s:os') },
          \ { 'type': 'files',     'header': ['  最近文档']       },
          \ { 'type': 'bookmarks', 'header': ['  自定书签']      },
	  \ { 'header': ['  设置'],        'type': function('s:setting') },
	  \ { 'header': ['  帮助'],        'type': function('s:helping') },
	  \ { 'header': ['  关于'],        'type': function('s:about') },
          \ ]

" 自定义书签
let g:startify_bookmarks = [ {'g': '~/write/novel/greentunnel.md'}, ]


