export ZSH="/home/amnesia/.config/zsh/.oh-my-zsh"

source $ZDOTDIR"/ex.sh"

autoload -U compinit && compinit
autoload -Uz promptinit && promptinit
#prompt_mytheme_setup() { PS1="%B%F{green}➜ %B%F{blue}%~ %f%b" }
#prompt_themes+=( rubbyshell )
#prompt mytheme

plugins=(git zsh-completions zsh-syntax-highlighting zsh-autosuggestions history-substring-search vi-mode)

ZSH_THEME="robbyrussell"
source $ZSH/oh-my-zsh.sh

alias r="ranger"
alias v="nvim"
alias vim="nvim"
alias e="emacsclient -c -a 'emacs'"
alias emacs="emacsclient -c -a 'emacs'"
alias cat="bat"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias dfs='dotfiles status'
alias dfc='dotfiles commit'
alias dfp='dotfiles push'
alias dfa='dotfiles add'
alias dfr='dotfiles rm'
alias neo='neo-matrix -D -s -c cyan'
alias ls="exa -l"
