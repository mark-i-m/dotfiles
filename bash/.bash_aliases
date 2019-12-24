alias sl="ls"

alias pyhton='python'
alias python='python2'

alias ...="cd .."
alias ....="cd ../.."
alias .....="cd ../../.."
alias cd..="cd .."

alias cdjnk="cd /tmp/"

function open {
    for file in $@ ; do
        xdg-open $file 2>&1 > /dev/null &
    done
}

alias date='date +"%a %b %d %Y %I:%M.%S %p"'

alias nv="nvim"
