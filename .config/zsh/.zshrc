export ZSH="/home/amnesia/.config/zsh/.oh-my-zsh"


autoload -U compinit && compinit
autoload -Uz promptinit && promptinit
#prompt_mytheme_setup() { PS1="%B%F{green}➜ %B%F{blue}%~ %f%b" }
#prompt_themes+=( rubbyshell )
#prompt mytheme

plugins=(git zsh-completions zsh-syntax-highlighting zsh-autosuggestions history-substring-search)

ZSH_THEME="robbyrussell"
source $ZSH/oh-my-zsh.sh

alias v="nvim"
alias vim="nvim"
#alias f="fzf_to_vim.sh"
#alias nc="nordvpn connect"
#alias ncb="nordvpn connect Brazil"
#alias nd="nordvpn disconnect"
#alias ls="/home/amnesia/.local/share/gem/ruby/3.0.0/gems/colorls-1.4.4/exe/colorls -A -1"
#alias cat="bat"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias dfs='dotfiles status'
alias dfc='dotfiles commit'
alias dfp='dotfiles push'
alias dfa='dotfiles add'
