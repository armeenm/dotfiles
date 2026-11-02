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

# Carapace's bash bridge compdefs every bash completion it finds, which would
# shadow native zsh completions for commands carapace has no completer of its
# own for. Snapshot the native set now (compinit runs from /etc/zshrc) and put
# it back once carapace has been sourced; see _carapace_restore_native_completions.
typeset -gA _native_completions
_native_completions=(${(kv)_comps})

d () {
  diff -u $@ | delta
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

autoload -Uz add-zsh-hook
_carapace_restore_native_completions() {
  add-zsh-hook -d precmd _carapace_restore_native_completions
  [[ -r "$CARAPACE_BASE_COMPLETERS" ]] || return

  local -A carapace_base
  local name fn
  while IFS= read -r name; do
    carapace_base[$name]=1
  done < "$CARAPACE_BASE_COMPLETERS"

  for name fn in ${(kv)_native_completions}; do
    [[ ${_comps[$name]} == _carapace_completer ]] || continue
    (( ${+carapace_base[$name]} )) && continue
    compdef "$fn" "$name"
  done
  unset _native_completions
}
add-zsh-hook precmd _carapace_restore_native_completions

[[ ! -f ~/.env ]] || source ~/.env
