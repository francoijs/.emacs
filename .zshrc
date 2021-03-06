# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000000
SAVEHIST=1000000
setopt INC_APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_DUPS
unsetopt beep
bindkey -e
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/francois/.zshrc'

# Completion
autoload -Uz compinit
compinit
# expand word left of cursor (!= word under cursor)
bindkey '^i' expand-or-complete-prefix

# Prompt
git_branch_name() {
  ref=$(git symbolic-ref HEAD 2> /dev/null | cut -d'/' -f3)
  echo $ref
}
setopt PROMPT_SUBST
NEWLINE=$'\n' PROMPT='${NEWLINE}%F{240}[%h] %F{cyan}%* %F{240}[%F{blue}%n@%m%F{240}] %F{white}%~ %F{blue}($(git_branch_name))%F{reset} ${NEWLINE}$ %F{reset}'

# deactivate 'suspend & resume' so that CTRL-S may be used to search forward in history
stty -ixon

# Tune shortcuts
# ctrl-left/right
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "\C-w" kill-region
# remove '/-.' from word characters
WORDCHARS='*?_[]~=&;!#$%^(){}<>'

# Aliases
alias ls='ls --color=auto'

# Java stuff
CLASSPATH=/usr/share/java/stringtemplate4.jar:/usr/share/java/antlr4.jar:/usr/share/java/antlr4-runtime.jar:/usr/share/java/antlr3-runtime.jar/:/usr/share/java/treelayout.jar

# ANTLR4
alias grun='/usr/share/antlr4/grun'
alias showme=tldr
alias vuze=biglybt
