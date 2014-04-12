# Colored output for various programs
alias cldiff='colordiff'
alias clmake='colormake'
alias clgcc='colorgcc'

# Shortcuts
alias l='ls -CF'
alias f='finger'
alias ll='ls -lh'
alias la='ls -alh'
alias ls='ls --color=auto '
alias ..='cd ..'
alias df='df -h'
alias g='git'
alias ack='ack-grep'
alias t="tmux -2"
alias ta="tmux attach"
alias v="vim"

# Package managers
alias apfs='apt-file search'
alias aps='apt-cache search'
alias apu='sudo apt-get update'
alias apug='sudo apt-get update && sudo apt-get dist-upgrade'
alias apui='sudo apt-get install'
alias xopen='xdg-open'

# Grep settings
alias grep='grep --color=auto --binary-files=without-match --devices=skip'

# Fix 256 colors in tmux
alias tmux='tmux -2'

# Programs
alias nb='newsbeuter'
alias mp='ncmpcpp'
alias nset='nvidia-settings'

# Mutt with different accounts
alias gmail='mutt -F ~/.muttrc.gmail'
