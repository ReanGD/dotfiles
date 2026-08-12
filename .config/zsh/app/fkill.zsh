# Resolve at source time, independently of the caller's working directory.
typeset -g _fprocess_helper="${${(%):-%N}:A:h}/fprocess.py"

_fsignal() {
  if ! command -v python3 >/dev/null 2>&1; then
    print -u2 -- 'fkill/fterm require python3'
    return 127
  fi
  command python3 "$_fprocess_helper" pick -- "$1"
}

# Signals target individual PIDs, never entire trees or compositor windows.
fkill() { _fsignal "${1:-KILL}"; }
fterm() { _fsignal "${1:-TERM}"; }
