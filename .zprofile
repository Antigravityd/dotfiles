# Honor system-wide environment variables
source /etc/profile

export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)


[[ -t 0 && $(tty) == /dev/tty2 && $- =~ "l" ]]  && source ~/.zshrc && exec startx
