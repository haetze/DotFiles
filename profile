
# MacPorts Installer addition on 2013-01-06_at_11:37:28: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.


##
# Your previous /Users/haetze/.profile file was backed up as /Users/haetze/.profile.macports-saved_2013-01-06_at_11:47:51
##

# MacPorts Installer addition on 2013-01-06_at_11:47:51: adding an appropriate PATH variable for use with MacPorts.
export PATH=/opt/local/bin:/opt/local/sbin:$PATH
# Finished adapting your PATH environment variable for use with MacPorts.

export PATH=/usr/local/bin:$PATH

export GOPATH=/Users/haetze/Documents/Google/apps/go

alias appleScript=osascript

function cd {
    # actually change the directory with all args passed to the function
        builtin cd "$@"
	# if there's a regular file named "todo.txt"...
	if [ -e ".git" ] ; then
		# display its contents
		git pull
	fi
}
