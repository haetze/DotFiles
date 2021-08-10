export JAVA_HOME;
export PATH="$HOME/.cargo/bin:$PATH"

export PATH="$HOME/.bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.go/bin:$PATH"
export GOPATH="$HOME/.go"
# opam configuration
test -r /Users/haetze/.opam/opam-init/init.sh && . /Users/haetze/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
alias cw="ncal -w | tail -1 | awk '{print $1}'"

