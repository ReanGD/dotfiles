_omp() (
  local omp_bin="${XDG_CACHE_HOME:-$HOME/.cache}/.bun/bin/omp"

  if (( $# == 0 )) || [[ -z "$1" ]]; then
    print -u2 'Использование: _omp <agent-profile> [--sandbox <sandbox-profile>] [аргументы omp...]'
    return 1
  fi

  local agent_profile="$1"
  shift

  local sandbox_profile=""

  if [[ "${1:-}" == "--sandbox" ]]; then
    if (( $# < 2 )) || [[ -z "${2:-}" ]]; then
      print -u2 'Ошибка: для --sandbox требуется имя профиля'
      return 1
    fi

    sandbox_profile="$2"
    shift 2
  fi

  local xdg_config_home="${XDG_CONFIG_HOME:-$HOME/.config}"

  local omp_root="$xdg_config_home/omp"
  local profiles_root="$omp_root/profiles"

  local permissions_file
  if [[ -n "$sandbox_profile" ]]; then
    permissions_file="$profiles_root/common/sandbox_permissions.yml"
  else
    permissions_file="$profiles_root/common/host_permissions.yml"
  fi

  local -a config_files=(
    "$profiles_root/common/ui.yml"
    "$profiles_root/common/model_settings.yml"
    "$permissions_file"
  )

  local file
  for file in \
    "$profiles_root/common/capabilities.yml" \
    "$profiles_root/$agent_profile/model_settings.yml" \
    "$profiles_root/$agent_profile/ui.yml" \
    "$profiles_root/$agent_profile/capabilities.yml" \
    "$profiles_root/$agent_profile/config.yml"
  do
    [[ -f "$file" ]] && config_files+=("$file")
  done

  local config_path="${(j.:.)config_files}"

  local -a env_args=(
    "LINK_BROWSER=chromium_ai"
    "LINK_FILTER=all"
    "AGENT_PROFILE=$agent_profile"
    "PI_TUI_RENDER_BACKEND=app-viewport"
    "PI_CONFIG_FILES=$config_path"
  )

  local -a command=(
    "$omp_bin"
    "$@"
  )

  if [[ -n "$sandbox_profile" ]]; then
    env_args+=(
      "HERDR_AGENT=omp"
    )

    # sh ждёт nono и сохраняет читаемый для Herdr HERDR_AGENT=omp; exit не даёт sh заменить себя защищённым процессом nono.
    command=(
      sh -c '"$@"; exit $?' sh
      nono run
      --profile "$sandbox_profile"
      --
      "${command[@]}"
    )
  fi

  _with_proxy_ai env \
    "${env_args[@]}" \
    "${command[@]}"
)

alias ocfg='_omp cfg --sandbox profile-cfg'
alias owork='_omp work --sandbox profile-work'
alias ohome='_omp home --sandbox profile-home'
alias omp='_omp host'
