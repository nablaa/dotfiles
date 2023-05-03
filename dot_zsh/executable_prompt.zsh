# Set colors for ZSH
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
	colors
fi

for color in RED GREEN YELLOW BLUE MAGENTA CYAN WHITE; do
	eval PR_$color='%{$terminfo[bold]$fg[${(L)color}]%}'
	eval PR_LIGHT_$color='%{$fg[${(L)color}]%}'
	(( count = $count + 1 ))
done

source ~/.zsh/git-prompt/git-prompt.zsh

PR_NO_COLOR="%{$terminfo[sgr0]%}"
PS1='[${PR_BLUE}%n\
${PR_WHITE}@\
${PR_GREEN}%U%m%u\
${PR_NO_COLOR} \
${PR_RED}%d\
${PR_NO_COLOR}]\
$(gitprompt)\
%# '

# Show text "-- NORMAL --" when the shell is in VI normal mode
function zle-line-init zle-keymap-select {
	RPS1="${${KEYMAP/vicmd/$PR_RED-- NORMAL --$PR_NO_COLOR}/(main|viins)/$TEMP_RPS1}"
	#RPS2=$RPS1
	zle reset-prompt
}
zle -N zle-line-init
zle -N zle-keymap-select
