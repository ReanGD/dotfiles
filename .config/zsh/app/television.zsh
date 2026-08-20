# Let channel actions request a directory change in the calling shell.
_tv() {
  local cd_file tv_status destination
  local -a destinations=()
  cd_file=$(mktemp "${TMPDIR:-/tmp}/tv-cd.XXXXXXXXXX") || return

  {
    (
      # Restore keyboard reporting even when tv is interrupted.
      printf '\033[>1u' >&2
      trap 'printf "\033[<u" >&2' EXIT
      TV_CD_FILE=$cd_file command tv "$@"
    )
    tv_status=$?

    if (( tv_status == 0 )) && [[ -s $cd_file ]]; then
      while IFS= read -r -d '' destination; do
        destinations+=("$destination")
      done < "$cd_file"
      if (( ${#destinations} != 1 )); then
        print -u2 -- 'tv: select exactly one directory for Ctrl-O'
        return 1
      fi
      builtin cd -- "${destinations[1]}" || return
    fi
    return $tv_status
  } always {
    command rm -f -- "$cd_file"
  }
}

alias tv='_tv'

# Complete channel names with ordinary Tab; Ctrl-Space uses the tv picker.
_tv_channel_completion() {
  if (( CURRENT == 2 )); then
    local -a channels
    channels=("${(@f)$(command tv list-channels)}")
    _describe 'channel' channels
  else
    _files
  fi
}
_post_compinit+=("compdef _tv_channel_completion tv _tv")

# Based on tv 0.15.9 init zsh: open tv on empty input and use --ui-scale 50.
# Ctrl-Space is bound in lib/key_bindings.zsh after it creates the rean keymap.
_disable_bracketed_paste() {
    # Check if bracketed paste is defined, for compatibility with older versions
    if [[ -n $zle_bracketed_paste ]]; then
        print -nr ${zle_bracketed_paste[2]} >${TTY:-/dev/tty}
    fi
}

_enable_bracketed_paste() {
    # Check if bracketed paste is defined, for compatibility with older versions
    if [[ -n $zle_bracketed_paste ]]; then
        print -nr ${zle_bracketed_paste[1]} >${TTY:-/dev/tty}
    fi
}

__tv_path_completion() {
  local base lbuf suffix tail dir leftover matches
  base=$1
  lbuf=$2
  suffix=""
  tail=" "

  eval "base=$base" 2> /dev/null || return
  [[ $base = *"/"* ]] && dir="$base"
  while [ 1 ]; do
    if [[ -z "$dir" || -d ${dir} ]]; then
      leftover=${base/#"$dir"}
      leftover=${leftover/#\/}
      [ -z "$dir" ] && dir='.'
      [ "$dir" != "/" ] && dir="${dir/%\//}"
      zle -I
      matches=$(
        shift
        # tv "$dir" --autocomplete-prompt "$lbuf" --inline --no-status-bar --input "$leftover" < /dev/tty | while read -r item; do
        tv "$dir" --autocomplete-prompt "$lbuf" --ui-scale 50 --no-status-bar --input "$leftover" < /dev/tty | while read -r item; do
          item="${item%$suffix}$suffix"
          dirP="$dir/"
          [[ $dirP = "./" ]] && dirP=""
          echo -n -E "$dirP${(q)item} "
        done
      )
      matches=${matches% }
      if [ -n "$matches" ]; then
        LBUFFER="$lbuf$matches$tail"
      fi
      zle reset-prompt
      break
    fi
    dir=$(dirname "$dir")
    dir=${dir%/}/
  done
}

_tv_smart_autocomplete() {
  _disable_bracketed_paste

  local tokens prefix trigger lbuf
  setopt localoptions noshwordsplit noksh_arrays noposixbuiltins

  # http://zsh.sourceforge.net/FAQ/zshfaq03.html
  # http://zsh.sourceforge.net/Doc/Release/Expansion.html#Parameter-Expansion-Flags
  tokens=(${(z)LBUFFER})
  if [ ${#tokens} -lt 1 ]; then
    # zle ${fzf_default_completion:-expand-or-complete}
    __tv_path_completion "" "$LBUFFER"
    return
  fi

  [[ ${LBUFFER[-1]} == ' ' ]] && tokens+=("")

  if [[ ${LBUFFER} = *"${tokens[-2]-}${tokens[-1]}" ]]; then
    tokens[-2]="${tokens[-2]-}${tokens[-1]}"
    tokens=(${tokens[0,-2]})
  fi

  lbuf=$LBUFFER
  prefix=${tokens[-1]}
  [ -n "${tokens[-1]}" ] && lbuf=${lbuf:0:-${#tokens[-1]}}

  __tv_path_completion "$prefix" "$lbuf"

  _enable_bracketed_paste
}

zle -N tv-smart-autocomplete _tv_smart_autocomplete
