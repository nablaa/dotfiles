# Shortcuts
alias l='ls -CF'
alias f='find'
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
alias apu='sudo apt update'
alias apug='sudo apt update && sudo apt full-upgrade'
alias apui='sudo apt install'
alias xopen='xdg-open'

# Grep settings
alias grep='grep --color=auto --binary-files=without-match --devices=skip'

# Fix 256 colors in tmux
alias tmux='tmux -2'

alias grm="git reset --hard origin/master"

alias p3="python3"

alias headmessage="git show -s --format=%B HEAD~1"

alias mysuspend="xscreensaver-command -lock; sleep 3; systemctl suspend"

alias rbuild="cargo build && cargo test && cargo clippy --all-features --tests -- -D warnings && cargo fmt"
