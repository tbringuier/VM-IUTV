#-------------------------#
#     Prompt with $?      #
#-------------------------#

if ! grep -q '\[$?\]' <<<"$PS1"; then
  export PS1='[$?] '"$PS1"
fi

#-------------------------#
#        Aliases          #
#-------------------------#

alias grep='grep --color=auto -d skip'
alias pstree='pstree -plnhut'

alias la="ls -lA"
alias lt="ls -lt"

alias pg='ps aux | grep'
alias dg='dpkg -l | grep'
alias hg='history | grep'

# Minor but noising bug ("GVFS-RemoteVolumeMonitor-WARNING" or similar):
alias gedit="gedit 2>/dev/null"
alias mousepad="mousepad 2>/dev/null"
alias meld="meld 2>/dev/null"

#-------------------------#
#          Env            #
#-------------------------#

[[ -n $USER    ]] || export USER=$(id -un)
[[ -n $LOGNAME ]] || export LOGNAME=$USER
[[ -n $EDITOR  ]] || export EDITOR="gedit 2>/dev/null"

# Set HOME according to the user:
if [[ $USER = root ]]; then HOME=/root; else HOME=/home/$USER; fi
# ---

# This command should be contained in ~/.bashrc
if [[ $USER = "root" && ! -f "/root/.Xauthority" ]]; then
  XAUTHORITY=$(find /home -type f -name ".Xauthority" | head -n 1)
  if [[ -f "$XAUTHORITY" ]]; then
    cp "$XAUTHORITY" /root/
  fi
fi

# JV: from vnc_startup.sh:
# ---
### Issue #7: Fixing problems with foreground mode
### (compensates the last 'WORKDIR ${STARTUPDIR}' in Dockerfile)
# ---
cd "$HOME"

# Load all useful definitions in $BASH_ENV like a non-interactive shell (why not?).
# (See: https://www.gnu.org/software/bash/manual/html_node/Bash-Startup-Files.html)
if [[ -n "$BASH_ENV" ]]; then
  source "$BASH_ENV";
  # Trace this source call:
  trace_bash_call || true
fi
