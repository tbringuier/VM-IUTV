#!/bin/bash

# This file is part of Mariotel
# Copyright (C) 2020  Jean-Vincent Loddo
# Copyright (C) 2020  Université Sorbonne Paris Nord
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# ---------------------------------------------------------------------
#                         License note
# ---------------------------------------------------------------------
# This script is an heavely modified version of the original source,
# provided without a specific licence neither copyright specifications:
# ---
#   Location : https://github.com/accetto/ubuntu-vnc-xfce.git
#   File     : ./src/startup/vnc_startup.sh
#   License  : Apache 2.0 (specified in ./LICENSE for the whole project)
# ---
# This modified version is released under GPL-v3 as allowed by the Apache
# license. Other unmodified sources of the original code of the accetto's
# git project are still released with the Apache License.
# ---------------------------------------------------------------------

### every exit != 0 fails the script
set -e

#-------------------------#
#         help            #
#-------------------------#

# JV: TO BE DELETED

## print out help
help (){
echo "
USAGE:
docker run <run-options> accetto/<image>:<tag> <option> <optional-command>

IMAGES:
accetto/ubuntu-vnc-xfce

TAGS:
latest      based on 'latest' Ubuntu
rolling     based on 'rolling' Ubuntu

OPTIONS:
-w, --wait      (default) Keeps the UI and the vnc server up until SIGINT or SIGTERM are received.
                An optional command can be executed after the vnc starts up.
                example: docker run -d -P accetto/ubuntu-vnc-xfce
                example: docker run -it -P --rm accetto/ubuntu-vnc-xfce bash

-s, --skip      Skips the vnc startup and just executes the provided command.
                example: docker run -it -P --rm accetto/ubuntu-vnc-xfce --skip echo $BASH_VERSION

-d, --debug     Executes the vnc startup and tails the vnc/noVNC logs.
                Any parameters after '--debug' are ignored. CTRL-C stops the container.
                example: docker run -it -P --rm accetto/ubuntu-vnc-xfce --debug

-t, --tail-log  same as '--debug'

-h, --help      Prints out this help.
                example: docker run --rm accetto/ubuntu-vnc-xfce

Fore more information see: https://github.com/accetto/ubuntu-vnc-xfce
"
}
if [[ $1 =~ -h|--help ]]; then help; exit 0; fi

#-------------------------#
#          Env            #
#-------------------------#

export STARTUPDIR="${STARTUPDIR:-/dockerstartup}"
export BASH_ENV=$STARTUPDIR/bash_env.sh
# source ${BASH_ENV} # as for all descendants

[[ -n $USER    ]] || export USER=$(id -un)
[[ -n $LOGNAME ]] || export LOGNAME=$USER

# Set HOME according to the user:
if [[ $USER = root ]]; then HOME=/root; else HOME=/home/$USER; fi
# ---

# The relevant log file for Mariotel:
LOGFILE="${STARTUPDIR}/no_vnc_startup.log"
export LOGFILE
# ---
set +x
exec 2>>$LOGFILE
# ---

# Non empty values of this variable activate the SSL mode:
if [[ -n $NO_VNC_SSLONLY && -r ${NO_VNC_HOME}/self.pem ]]; then
  NO_VNC_SSLONLY='--ssl-only'
else
  unset NO_VNC_SSLONLY
fi

#-------------------------#
#       nss_wrapper       #
#-------------------------#

# ---
# From $STARTUPDIR/generate_container_user
# ---
# Set current user in nss_wrapper
if [[ "$(id -u)" != 0 ]] ; then
    # ---
    export NSS_WRAPPER_PASSWD=/etc/passwd
    export NSS_WRAPPER_GROUP=/etc/group
    # ---
    LIBNSS="libnss_wrapper.so"
    if   [[ -r /usr/lib/$LIBNSS   ]]; then LD_PRELOAD=/usr/lib/$LIBNSS;
    elif [[ -r /usr/lib64/$LIBNSS ]]; then LD_PRELOAD=/usr/lib64/$LIBNSS;
    else
        LD_PRELOAD=$(find /usr -name "$LIBNSS" | head -n 1)
        if [[ -z $LD_PRELOAD ]]; then
          echo "no libnss_wrapper.so installed!"
          exit 1
        fi
    fi
    # ---
    export LD_PRELOAD
fi

### Issue #7: Fixing problems with foreground mode
### (compensates the last 'WORKDIR ${STARTUPDIR}' in Dockerfile)
cd "$HOME"

#-------------------------#
#      User limits        #
#-------------------------#

# No core dumps please:
ulimit -c 0

# ---
# DON'T SET THE FILE SIZE LIMITS: this kind of user limit (-f) causes
# serious problems to User Mode Linux (UML), hence to Marionnet.
# ---
# File size in Kbytes must not exceed 2/3 of the (virtual) disk space:
# BLOCKS=$(($(df -k --output=avail / | tail -n 1)*2/3))
# ulimit -f $BLOCKS
# ---

#-------------------------#
#   Tunings and services  #
#-------------------------#

# Launch services as root
# (using `sudo' set to run without password for `mariotel_services'):
# --
function startup_mariotel_services {
  local TARGET="${STARTUPDIR}"/mariotel_services.sh
  # ---
  [[ -x "$TARGET" ]] || return
  set +e
  # ---
  echo "$(date +%Y-%m-%d.%H\h%M): vnc_startup.sh: about to call mariotel_services.sh" >> $LOGFILE
  sudo $TARGET
}
# --- Go:
startup_mariotel_services &
# ---

# We have to wait for `xfce4-panel` being launched:
function delayed_mariotel_tunings {
  local k=0
  local TIMEOUT=5 # max 5 seconds
  local TARGET="${STARTUPDIR}"/mariotel_tunings.sh
  # ---
  [[ -e "$TARGET" ]] || return
  set +e
  # ---
  while [[ $k -lt $TIMEOUT ]] && ! pgrep xfce4-panel &>/dev/null; do sleep 1s; let k=k+1; done
  # ---
  echo "$(date +%Y-%m-%d.%H\h%M): vnc_startup.sh: about to call mariotel_tunings.sh" >> $LOGFILE
  bash -c "$TARGET"
}
# --- Go:
delayed_mariotel_tunings &
# ---

#-------------------------#
#          skip           #
#-------------------------#

### add `--skip` to startup args, to skip the VNC startup procedure
if [[ $1 =~ -s|--skip ]]; then
    echo "Skip VNC startup"
    echo "Executing command: '${@:2}'"
    exec "${@:2}"
fi

if [[ $1 =~ -d|--debug ]]; then
    echo "Debug VNC startup"
    export DEBUG=true
fi

#-------------------------#
#         trap            #
#-------------------------#

# Usage: trap_with METHOD SIGSPEC...
function trap_with {
  local METHOD="$1"
  shift
  # ---
  local SIGNAL
  for SIGNAL in "$@"; do
    trap "$METHOD $SIGNAL" $SIGNAL
  done
}

# Ignore all signals:
function ignore_signal {
  echo "$(date +%Y-%m-%d.%H\h%M): Received signal $1 (ignored)" >> $LOGFILE
}

# Ignore all signals:
trap_with ignore_signal {1..8} {10..17} {20..64}
# ---
# We can test the resilience of this script (PID 1) from a console with:
# for i in {1..8} {10..17} {20..64}; do kill -$i 1; done

# Notify exiting:
trap "echo \"$(date +%Y-%m-%d.%H\h%M): Exiting...\" >> $LOGFILE" EXIT

# Show trapped signals:
# { echo -e "trap -p\n---\n"; trap -p; } >> $LOGFILE

#-------------------------#
#    stop containers in   #
#    inconsistent state   #
#-------------------------#

# No way to run correctly without Xvnc and the related stuff:
function pstree_looks_roughly_correct {
  local ERROR_CODE=3 # No such process
  if pgrep Xvnc && pgrep xfce4-panel && pgrep launch && pgrep xfce4-session; then
   return 0
  else
    return $ERROR_CODE
  fi &>/dev/null
}
export -f pstree_looks_roughly_correct

# ---
function check_pstree_every_now_and_then {
  # ---
  set +e
  trap true {1..8} {10..17} {20..64}
  # ---
  # At the starting time, we have to wait for something consistent:
  while ! pstree_looks_roughly_correct; do sleep 2s; done
  # ---
  # After that we will check every 10 seconds:
  local FREQUENCE=10
  # ---
  while pstree_looks_roughly_correct; do sleep ${FREQUENCE}s; done
  # ---
  echo "$(date +%Y-%m-%d.%H\h%M): vnc_startup.sh: process tree doesn't look roughly correct!" >> $LOGFILE
  kill 1 # i.e. vnc_startup.sh
}
export -f check_pstree_every_now_and_then
# --- Go:
(exec -a "pstree-monitor" bash -c check_pstree_every_now_and_then)&
# ---

#-------------------------#
#      VNC config         #
#-------------------------#

### resolve_vnc_connection
VNC_IP=$(hostname -i)

if [[ ${DEBUG} ]] ; then
    echo "DEBUG:"; id
    echo "DEBUG: ls -la /"; ls -la /
    echo "DEBUG: ls -la ."; ls -la .
fi

# --
# About X11:
sudo su -c 'rm -f /tmp/.X1-lock'
sudo su -c 'rm -rf /tmp/.X11-unix/'
# ---

### change vnc password
### first entry is control, second is view (if only one is valid for both)
mkdir -p "${HOME}"/.vnc
PASSWD_PATH="${HOME}/.vnc/passwd"

if [[ "${VNC_VIEW_ONLY}" == "true" ]]; then
    echo "Start VNC server in view only mode"
    ### create random pw to prevent access
    echo $(head /dev/urandom | tr -dc A-Za-z0-9 | head -c 20) | vncpasswd -f > "${PASSWD_PATH}"
fi

echo "${VNC_PW}" | vncpasswd -f >> "${PASSWD_PATH}"
chmod 600 "${PASSWD_PATH}"

#-------------------------#
#       VNC start         #
#-------------------------#

### start vncserver and noVNC webclient in the background
echo "Starting noVNC"
"${NO_VNC_HOME}"/utils/launch.sh --vnc localhost:${VNC_PORT} ${NO_VNC_SSLONLY} --listen ${NO_VNC_PORT} &>> $LOGFILE &
PID_SUB=$!

echo "Starting VNC server ..."
echo "... remove old VNC locks to be a reattachable container"
vncserver -kill ${DISPLAY} &> "${STARTUPDIR}"/vnc_startup.log \
    || rm -rfv /tmp/.X*-lock /tmp/.X11-unix &> "${STARTUPDIR}"/vnc_startup.log \
    || echo "... no locks present"

echo "... VNC params: VNC_COL_DEPTH=${VNC_COL_DEPTH}, VNC_RESOLUTION=${VNC_RESOLUTION}"
echo "... VNC params: VNC_BLACKLIST_TIMEOUT=${VNC_BLACKLIST_TIMEOUT}, VNC_BLACKLIST_THRESHOLD=${VNC_BLACKLIST_THRESHOLD}"
vncserver ${DISPLAY} -depth ${VNC_COL_DEPTH} -geometry ${VNC_RESOLUTION} \
    -BlacklistTimeout ${VNC_BLACKLIST_TIMEOUT} \
    -BlacklistThreshold ${VNC_BLACKLIST_THRESHOLD} &>> $LOGFILE

### log connect options
echo "... VNC server started on display ${DISPLAY}"
echo "Connect via VNC viewer with ${VNC_IP}:${VNC_PORT}"
echo "Connect via noVNC with http://${VNC_IP}:${NO_VNC_PORT}"

if [[ ${DEBUG} == true ]] || [[ $1 =~ -t|--tail-log ]]; then
    echo "Display log: ${HOME}/.vnc/*${DISPLAY}.log"
    ### if option `-t` or `--tail-log` block the execution and tail the VNC log
    tail -f "${STARTUPDIR}"/*.log "${HOME}"/.vnc/*"${DISPLAY}".log
fi

#-------------------------#
#       Main loop         #
#-------------------------#

# Stop with the mode -e right now (failures of the `wait'
# command, provoked by signals, must be allowed):
set +e
exec 2>&1

# Support to prevent the exit from this script when a signal is received:
function main_loop {
  local EXIT_RIGHT_NOW
  while [[ $EXIT_RIGHT_NOW != y ]]; do
    # echo "main_loop: about to wait..." >> $LOGFILE
    # ---
    wait ${PID_SUB} && EXIT_RIGHT_NOW=y
    # ---
    # No way to run correctly without Xvnc and the related stuff:
    pstree_looks_roughly_correct || exit 3  # No such process
    # ---
  done
  echo "main_loop: exiting..." >> $LOGFILE
}

# ---
if [ -z "$1" ] || [[ $1 =~ -w|--wait ]]; then
    main_loop
else
    ### unknown option ==> call command
    echo "Executing command: '$@'"
    exec "$@"
fi
