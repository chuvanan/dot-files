
# solarized terminal
eval (dircolors ~/.dir_colors/dircolors | head -n 1 | sed 's/^LS_COLORS=/set -x LS_COLORS /;s/;$//')

alias sau="sudo apt update"
alias sag="sudo apt upgrade"
alias sai="sudo apt install"
alias srb="sudo reboot"
alias saar="sudo apt autoremove"
alias saac="sudo apt autoclean"
alias sshn="sudo shutdown -h now"

# Caps Lock as Ctrl
alias scasc="setxkbmap -option ctrl:nocaps"

# A better cat
alias cat="bat"

# tldr
alias help='tldr'
