#!/bin/zsh

export PATH=$HOME/.local/bin:$HOME/scripts:$HOME/.local/share/neovim/bin:$HOME/.local/share/cargo/bin:$HOME/.cabal/bin:$HOME/.emacs.d/bin:$PATH

export TERMINAL=alacritty
export BROWSER=brave
export EDITOR=nvim
export VISUAL=nvim

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.local/cache"
export XDG_DESKTOP_DIR="$HOME/"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_VIDEOS_DIR="$HOME/downloads"
export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/X11/xinitrc"
export LESSHISTFILE="-"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export ERRFILE="${XDG_DATA_HOME:-$HOME/.local/share}/xsession-errors"

[[ $(pidof xinit) == "" ]] && startx $XINITRC
