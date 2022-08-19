export ZSH="/home/amnesia/.config/zsh/.oh-my-zsh"

source $ZDOTDIR"/cd.sh"
source $ZDOTDIR"/ex.sh"

autoload -U compinit && compinit
autoload -Uz promptinit && promptinit
#prompt_mytheme_setup() { PS1="%B%F{green}➜ %B%F{blue}%~ %f%b" }
#prompt_themes+=( rubbyshell )
#prompt mytheme

plugins=(git zsh-completions zsh-syntax-highlighting zsh-autosuggestions history-substring-search vi-mode)

ZSH_THEME="robbyrussell"
source $ZSH/oh-my-zsh.sh

eval $(thefuck --alias)

alias r="ranger"
alias v="nvim"
alias vim="nvim"
alias e="emacsclient -c -a 'emacs'"
alias cat="bat"
alias grep="rg"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias dfs='dotfiles status'
alias dfc='dotfiles commit'
alias dfp='dotfiles push'
alias dfa='dotfiles add'
alias dfr='dotfiles rm'
alias neo='neo-matrix -D -s -c cyan'
alias ls="exa --icons -l"
alias keys="xev | awk -F'[ )]+' '/^KeyPress/ { a[NR+2] } NR in a { printf \"%-3s %s\n\", \$5, \$8 }'"
