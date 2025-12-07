
#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="$HOME/.sdkman"

# Configure JAVA_HOME in bash sessions as well.
_setup_java_home() {
  if [[ -n "${JAVA_HOME:-}" ]]; then
    return
  fi

  if [[ -n "${SDKMAN_DIR:-}" && -d "${SDKMAN_DIR}/candidates/java/current" ]]; then
    export JAVA_HOME="${SDKMAN_DIR}/candidates/java/current"
    return
  fi

  case "${OSTYPE:-}" in
    darwin*)
      if [[ -x /usr/libexec/java_home ]]; then
        local detected
        detected=$(/usr/libexec/java_home 2>/dev/null || true)
        if [[ -n $detected ]]; then
          export JAVA_HOME="$detected"
          return
        fi
      fi
      ;;
  esac

  local java_path
  java_path="$(command -v java 2>/dev/null || true)"
  if [[ -n $java_path ]]; then
    local java_bin_dir
    java_bin_dir="$(dirname "$java_path")"
    if [[ $java_bin_dir == */bin ]]; then
      export JAVA_HOME="${java_bin_dir%/bin}"
    fi
  fi
}
_setup_java_home
unset -f _setup_java_home

[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"
