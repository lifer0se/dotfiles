export ZSH="/home/amnesia/.config/zsh/.oh-my-zsh"


autoload -U compinit && compinit
autoload -Uz promptinit && promptinit
prompt_mytheme_setup() { PS1="%B%F{green}➜ %B%F{blue}%~ %f%b" }
prompt_themes+=( mytheme )
prompt mytheme

plugins=(zsh-completions zsh-syntax-highlighting zsh-autosuggestions history-substring-search)

source $ZSH/oh-my-zsh.sh

alias ls="ls -a --color=auto"
alias f="fzf_to_vim.sh"
alias nc="nordvpn connect"
alias ncb="nordvpn connect Brazil"
alias nd="nordvpn disconnect"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
