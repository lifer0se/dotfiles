# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.config/zsh/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

export ZSH="/home/amnesia/.config/zsh/.oh-my-zsh"

source $ZDOTDIR"/cdls.sh"
source $ZDOTDIR"/shell_automatic_cd.sh"

autoload -U compinit && compinit
autoload -Uz promptinit && promptinit
#prompt_mytheme_setup() { PS1="%B%F{green}➜ %B%F{blue}%~ %f%b" }
#prompt_themes+=( rubbyshell )
#prompt mytheme

plugins=(git zsh-completions zsh-syntax-highlighting zsh-autosuggestions history-substring-search vi-mode)

ZSH_THEME="powerlevel10k/powerlevel10k"
source $ZSH/oh-my-zsh.sh


alias r="ranger_cd"
alias f="ranger_cd"
alias v="nvim"
alias vim="nvim"
alias e="emacsclient -c -a 'emacs'"
alias cat="bat"
alias dmenu="bat"
alias dotfiles='/usr/bin/git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'
alias dfs='dotfiles status'
alias dfc='dotfiles commit'
alias dfp='dotfiles push'
alias dfa='dotfiles add'
alias dfr='dotfiles rm'
alias neo='neo-matrix -D -s -c cyan'
alias ls="exa --icons -l"
alias xevs="xev | awk -F'[ )]+' '/^KeyPress/ { a[NR+2] } NR in a { printf \"%-3s %s\n\", \$5, \$8 }'"

# To customize prompt, run `p10k configure` or edit ~/.config/zsh/.p10k.zsh.
[[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh

[ -f "/home/amnesia/.local/share/ghcup/env" ] && source "/home/amnesia/.local/share/ghcup/env" # ghcup-env
