source /etc/zsh_command_not_found
source $HOME/.zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source $HOME/.autojump/etc/profile.d/autojump.zsh

# More colors for "ls" command
eval `dircolors -b`

# umask for -rwxr-xr-x
umask 022

GREP_OPTIONS="--color=auto"

# R - Raw color codes in output (don't remove color codes)
# S - Don't wrap lines, just cut off too long text
# M - Long prompts ("Line X of Y")
# ~ - Don't show those weird ~ symbols on lines after EOF
# g - Highlight results when searching with slash key (/)
# I - Case insensitive search
# s - Squeeze empty lines to one
# w - Highlight first line after PgDn
export LESS="-RSM~gIsw"

# Less Colors for Man Pages
export LESS_TERMCAP_mb=$'\E[01;31m'       # begin blinking
export LESS_TERMCAP_md=$'\E[01;38;5;74m'  # begin bold
export LESS_TERMCAP_me=$'\E[0m'           # end mode
export LESS_TERMCAP_se=$'\E[0m'           # end standout-mode
export LESS_TERMCAP_so=$'\E[01;31m'       # begin standout-mode - info box
export LESS_TERMCAP_ue=$'\E[0m'           # end underline
export LESS_TERMCAP_us=$'\E[01;32m'       # begin underline

# Autoload zsh modules when they are referenced
zmodload -a zsh/zprof zprof

# Shell options
setopt notify
setopt globdots
setopt correct
setopt pushdtohome
setopt longlistjobs
setopt histignoredups
setopt rcquotes
setopt mailwarning

# Key settings
bindkey -v
bindkey '^a' beginning-of-line
bindkey '^e' end-of-line
bindkey "^r" history-incremental-search-backward
bindkey ' ' magic-space    # also do history expansion on space
bindkey '^I' complete-word # complete on tab, leave expansion to _expand
bindkey '^[[3~' delete-char
bindkey '^[3;5~' delete-char
bindkey '^[[7~' beginning-of-line
bindkey '^[[8~' end-of-line
bindkey -M viins 'jj' vi-cmd-mode

# Special keys
# create a zkbd compatible hash;
# to add other keys to this hash, see: man 5 terminfo
typeset -A key

key[Home]=${terminfo[khome]}
key[End]=${terminfo[kend]}
key[Insert]=${terminfo[kich1]}
key[Delete]=${terminfo[kdch1]}
key[Up]=${terminfo[kcuu1]}
key[Down]=${terminfo[kcud1]}
key[Left]=${terminfo[kcub1]}
key[Right]=${terminfo[kcuf1]}
key[PageUp]=${terminfo[kpp]}
key[PageDown]=${terminfo[knp]}

for k in ${(k)key} ; do
	# $terminfo[] entries are weird in ncurses application mode...
	[[ ${key[$k]} == $'\eO'* ]] && key[$k]=${key[$k]/O/[}
done
unset k

[[ -n "${key[Up]}"      ]]  && bindkey  "${key[Up]}"      up-line-or-history
[[ -n "${key[Down]}"    ]]  && bindkey  "${key[Down]}"    down-line-or-history

# Show currently running program name as terminal title
if [ "$SHELL" = '/usr/bin/zsh' ]
then
	case $TERM in
		rxvt|*term)
			precmd() { print -Pn "\e]0;%m:%~\a" }
			preexec () { print -Pn "\e]0;$1\a" }
			;;
	esac
fi

# Show current directory in terminal status bar
chpwd () {
	[[ -o interactive ]] || return
	case $TERM in
		sun-cmd) print -Pn "\e]l%~\e\\"
			;;
		*xterm*|rxvt|(dt|k|E)term) print -Pn "\e]2;%~\a"
			;;
	esac
}

# Syntax highlight options
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor root)
