[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return # prevent TRAMP issues

autoload -U compinit bashcompinit
compinit
bashcompinit
alias ls='ls --color=auto'
alias ll='ls -al'
alias ..='cd ..'
alias ...='cd ../../'
alias .4='cd ../../..'
alias grep='grep --color=auto'
alias mkdir='mkdir -pv'
export EDITOR='emacsclient -a "" '
export VISUAL='emacsclient -a ""'
export PAGER=/bin/less

function preexec() {
  timer=$(date +%s%3N)
}

function precmd() {
  if [ $timer ]; then
    local now=$(date +%s%3N)
    local d_ms=$(($now-$timer))
    local d_s=$((d_ms / 1000))
    local ms=$((d_ms % 1000))
    local s=$((d_s % 60))
    local m=$(((d_s / 60) % 60))
    local h=$((d_s / 3600))
    if ((h > 0)); then elapsed=${h}h${m}m
    elif ((m > 0)); then elapsed=${m}m${s}s
    elif ((s >= 10)); then elapsed=${s}.$((ms / 100))s
    elif ((s > 0)); then elapsed=${s}.$((ms / 10))s
    else elapsed='' #elapsed=${ms}ms
    fi

    export RPROMPT="%F{240}${elapsed} %f"
    unset timer
  fi
}

PROMPT='%F{blue}%2~%f%(?..%F{88} %?%f) %F{magenta}%Bᛋ%b%f '

# export PHITSPATH=/home/dnw/phits326A/phits
# export PATH=/home/dnw/phits326A/phits/bin:${PATH}
# export PATH=/home/dnw/phits326A/phits/dchain-sp/bin:${PATH}
export PATH=/home/dnw/Code/bin:${PATH}
