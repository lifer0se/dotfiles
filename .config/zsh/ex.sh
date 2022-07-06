### ARCHIVE EXTRACTION
# usage: ex <file>
ex ()
{
    if [ -d "$2" ]; then
        CWD=$2
	cd $CWD
    else
        CWD=$pwd
    fi

    echo "Extracting: $1 to $2"
    if [ -f "$1" ] ; then
        case $1 in
            *.tar.bz2)   echo $(tar xjf $1)   ;;
            *.tar.gz)    echo $(tar xzf $1)   ;;
            *.bz2)       echo  $(bunzip2 $1)   ;;
            *.rar)       echo  $(unrar x $1)   ;;
            *.gz)        echo  $(gunzip $1)    ;;
            *.tar)       echo  $(tar xf $1)    ;;
            *.tbz2)      echo  $(tar xjf $1)   ;;
            *.tgz)       echo  $(tar xzf $1)   ;;
            *.zip)       echo  $(unzip $1)     ;;
            *.Z)         echo  $(uncompress $1);;
            *.7z)        echo  $(7z x $1)      ;;
            *.deb)       echo  $(ar x $1)      ;;
            *.tar.xz)    echo  $(tar xf $1)    ;;
            *.tar.zst)   echo  $(unzstd $1)    ;;
            *)           echo "'$1' cannot be extracted via ex()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
    echo "Done."
}
