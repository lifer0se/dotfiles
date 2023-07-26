#!/bin/zsh

export PATH=$HOME/.local/bin:$HOME/Scripts:$HOME/.local/share/neovim/bin:$HOME/.local/share/cargo/bin:$HOME/.cabal/bin:$PATH

export TERMINAL=alacritty
export BROWSER=firefox
export EDITOR="nvim"
export VISUAL="nvim"

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.local/cache"
export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DESKTOP_DIR="$HOME/"
export XDG_DOWNLOAD_DIR="$HOME/downloads"
export XDG_VIDEOS_DIR="$HOME/downloads"

export CABAL_CONFIG="$XDG_CONFIG_HOME"/cabal/config
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc-2.0

export ANDROID_HOME="$XDG_DATA_HOME"/android
export CABAL_DIR="$XDG_DATA_HOME"/cabal
export GNUPGHOME="$XDG_DATA_HOME"/gnupg
export PASSWORD_STORE_DIR="$XDG_DATA_HOME"/pass
export RUSTUP_HOME="$XDG_DATA_HOME"/rustup
export STACK_ROOT="$XDG_DATA_HOME"/stack
export WINEPREFIX="$XDG_DATA_HOME"/wine


export CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv
export NUGET_PACKAGES="$XDG_CACHE_HOME"/NuGetPackages

export HISTFILE="$XDG_STATE_HOME"/zsh/history

export GHCUP_USE_XDG_DIRS=true


alias svn="svn --config-dir $XDG_CONFIG_HOME/subversion"
alias wget=wget --hsts-file="$XDG_DATA_HOME/wget-hsts"

export XINITRC="${XDG_CONFIG_HOME:-$HOME/.config}/X11/xinitrc"
export LESSHISTFILE="-"

export QT_QPA_PLATFORMTHEME=qt5ct

export GCM_CREDENTIAL_STORE="cache"

xrdb -load "$XDG_CONFIG_HOME/X11/xresources"
