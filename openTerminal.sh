read pathname
DIRNAME=$(dirname $pathname)


if [ -z $pathname ] ; then
    exit
fi

if [ -f $pathname ] ; then
    xterm -fa 'Monospace' -fn 12x24 -e "cd ${DIRNAME} && zsh" ;
    
else
    if [ -d $pathname ] ; then
	xterm -fa 'Monospace' -fn 12x24 -e "cd ${pathname} && zsh" ;
	
    else
	case "$pathname" in
	    www*) chrome "${pathname}" ;;
	    *)    chrome "www.google.com/search?q=${pathname}" ;;
	esac
	
    fi
fi
