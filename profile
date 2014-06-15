
export PATH=/usr/local/bin:$PATH

export GOPATH=~/Documents/Code/go/DemoGO

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
