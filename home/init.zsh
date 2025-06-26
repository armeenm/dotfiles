function zshaddhistory() { return 1 }

bindkey '^ ' autosuggest-accept
_zsh_autosuggest_strategy_atuin_top() {
  suggestion=$(atuin search --cmd-only --limit 1 --search-mode prefix $1)
}

ZSH_AUTOSUGGEST_STRATEGY=atuin_top

autoload -Uz edit-command-line

zle-keymap-select () {
  if [ $KEYMAP = vicmd ]; then
    printf "\033[2 q"
  else
    printf "\033[6 q"
  fi
}

zle -N zle-keymap-select

zle-line-init () {
  zle -K viins
  printf "\033[6 q"
}

zle -N zle-line-init

zle -N edit-command-line
bindkey -M vicmd v edit-command-line
bindkey -v '^?' backward-delete-char

setopt globdots
setopt autopushd

d () {
  diff -u $@ | delta
}

function y() {
  local tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"
  if cwd="$(command cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
    builtin cd -- "$cwd"
  fi
  rm -f -- "$tmp"
}

function precmd {
  print -Pn "\e]133;A\e\\"
  if ! builtin zle; then
    print -n "\e]133;D\e\\"
  fi
}

function preexec {
  print -n "\e]133;C\e\\"
  print -Pn "\e]0;${(q)1}\e\\"
}

function scroll-top() {
  local esc
  local -i ROW COL OFFSET
  IFS='[;' read -sdR $'esc?\e[6n' ROW COL <$TTY
  OFFSET="${#${(@Af)PREBUFFER%$'\n'}}"+"${#${(@Af)LBUFFER:-1}}"
  (( ROW-OFFSET )) && printf '\e[%1$dS\e[%1$dA' ROW-OFFSET
  zle redisplay
}
zle -N clear-screen scroll-top

[[ ! -f ~/.env ]] || source ~/.env
