# Honor system-wide environment variables
source /etc/profile

[[ -t 0 && $(tty) == /dev/tty2 && $- =~ "l" ]]  && source ~/.zshrc && exec startx
