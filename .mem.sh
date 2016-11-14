available=$(cat /var/run/dmesg.boot | grep "avail memory" | sed 's/.*(//' | rev | cut -c 2- | rev)
pages=$(vmstat -s | grep "pages free" | ./.mem | rev | cut -c 11- | rev)
free=$(($pages*$(vmstat -s | grep "bytes per page" | rev | cut -c 15- | rev)/1000000))
used=$(($( echo "${available}" | rev | cut -c 4- | rev  )-$free))

echo "${used} MB of ${available} used "

	    
	    
	    
	    
