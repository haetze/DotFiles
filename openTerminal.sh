read pathname
DIRNAME=$(dirname $pathname)

if [ -f $pathname ] ; then
    xterm -fa 'Monospace' -fn 12x24 -e "cd ${DIRNAME} && zsh" ;
    
else
    if [ -d $pathname ] ; then
	xterm -fa 'Monospace' -fn 12x24 -e "cd ${pathname} && zsh" ;
	
    else
	chrome "www.google.com/search?q=${pathname}"
	
    fi
fi
