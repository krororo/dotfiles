augroup HighlightTrailingSpaces
  autocmd!
  autocmd VimEnter,WinEnter,ColorScheme * highlight TrailingSpaces term=underline guibg=Red ctermbg=Red
  autocmd VimEnter,WinEnter * match TrailingSpaces /\s\+$/
augroup END

set list
set listchars=tab:>.
hi Comment ctermfg=1
set expandtab
set shiftwidth=2

imap <C-a> <Home>
imap <C-e> <End>
imap <C-b> <Left>
imap <C-f> <Right>
imap <C-n> <Down>
imap <C-p> <UP>

imap <C-k> <ESC>d$i
imap <C-y> <ESC>pi
imap <C-d> <Del>
imap <C-h> <BS>
