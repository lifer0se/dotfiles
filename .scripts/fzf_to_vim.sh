#!/bin/sh

FZF_PATH=~/.config
[[ -d $1 ]] && FZF_PATH=$1

VIM_PATH=`find $FZF_PATH | fzf --prompt="➜ " --pointer="➜" --history=/home/amnesia/.config/zsh/.fzf_history --history-size=10000`

[[ -f $VIM_PATH ]] && vim -o $VIM_PATH
