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

# This file should be sourced and this should be happen automatically
# by setting somewhere early in the process hierarchy the environment
# variable BASH_ENV.
# ---
# Example:
#   export BASH_ENV=/etc/bash_env.sh
# ---

# Do not read this file several times:
[[ -z $BASH_ENV_SOURCED ]] || return
export -n BASH_ENV_SOURCED=true

#-----------------------------#
#          Tracing            #
#-----------------------------#

# ---
function am_i_sourced {
  test ! "$BASH_ARGV0" = "$BASH_SOURCE";
}
export -f am_i_sourced

# ---
# Trace a script call (sourced or not) on $1 or (global) $LOGFILE or $HOME/.bash.log
# Should be used at the beginning of a (sourced) script.
# ---
# Usage:
#   trace_bash_call [LOGFILE]
# ---
function trace_bash_call {
  local LOGFILE=${1:-$LOGFILE}
  LOGFILE=${LOGFILE:-$HOME/.bash.log}
  # ---
  local MODE
  if am_i_sourced; then MODE="source-ing"; else MODE="running"; fi
  # ---
  local MSG="$(date +%Y-%m-%d.%H:%M:%S): PPID $PPID PID $$ $MODE $BASH_SOURCE"
  echo "$MSG" >> $LOGFILE
}
export -f trace_bash_call

# Call it:
# trace_bash_call

#-------------------------#
#       signals           #
#-------------------------#

# Ignore but log:
function ignore_signal {
  local LOGFILE=${1:-$LOGFILE}
  LOGFILE=${LOGFILE:-$HOME/.bash.log}
  echo "$(date +%Y-%m-%d.%H:%M:%S): Received signal $1 (ignored)" >> $LOGFILE
}
export -f ignore_signal

# Usage: trap_with METHOD SIGSPEC...
# ---
# Example (ignore all signals):
#   trap_with ignore_signal {1..8} {10..17} {20..64}
# ---
function trap_with {
  local METHOD="$1"
  shift
  # ---
  local SIGNAL
  for SIGNAL in "$@"; do
    trap "$METHOD $SIGNAL" $SIGNAL
  done
}
export -f trap_with

#-----------------------------#
#  netstat related functions  #
#-----------------------------#

# TCP listening ports:
function tcp_ports {
 local CMD
 CMD="sudo netstat -tlnp"
 if [[ $# = 0 ]]; then
   $CMD
 else
   local ARGS="$@";
   $CMD | \grep "${ARGS// /\\|}"
 fi
}
export -f tcp_ports

# UDP waiting ports:
function udp_ports {
 local CMD
 CMD="sudo netstat -unpa"
 if [[ $# = 0 ]]; then
   $CMD
 else
   local ARGS="$@";
   $CMD | \grep "${ARGS// /\\|}"
 fi
}
export -f udp_ports

# Listening unix ports:
function unix_ports {
 local CMD="sudo netstat -xnpa | \grep LISTENING"
 if [[ $# = 0 ]]; then
   eval $CMD
 else
   local ARGS="$@";
   eval $CMD | \grep "${ARGS// /\\|}"
 fi
}
export -f unix_ports

# Service ports (TCP, UDP or UNIX):
function srv_ports {
 echo "--- TCP listening ports"
 tcp_ports "$@"
 echo "--- UDP waiting ports"
 udp_ports "$@"
 echo "--- UNIX listening ports"
 unix_ports "$@"
}
export -f srv_ports

#-------------------------#
#         Stuff           #
#-------------------------#

# Like `xargs' but for functions.
# ---
# Example:
#   head -n 100 SOURCE | awk '{print $10, $11, $12, $14}' | xargsf myfunc
# ---
function xargsf {
  while read line; do eval "$@" "$line"; done;
}
export -f xargsf

# Usage:
#   which_has FILE
#   which_has COMMAND
# ---
function which_deb_package_has {
 [[ $# = 1 ]] || {
   echo -e "Usage: which_has (<FILE>|<COMMAND>)\nReturns the installed debian package providing FILE or COMMAND.";
   return 1; }
 local FILE=$(type -P $1)
 local DPKG=$(type -P dpkg)
 [[ -f $FILE ]] || { echo "whohas: file or command '$1' not found"; return 2; }
 [[ -f $DPKG ]] || { echo "dpkg not found: not a Debian-like distribution?"; return 3; }
 dpkg -S $FILE || dpkg -S $(realpath $FILE);
 }
# ---
alias which_has='which_deb_package_has'
# ---

# Usage:
#   opened_by COMMAND
# ---
# Examples:
#   opened_by bash
#   opened_by grep "sda3" /etc/fstab
# ---
function opened_by {
 local TMPFILE=$(mktemp)
 strace 2>$TMPFILE "$@"
 echo "Opened files:"
 \grep <$TMPFILE -e "^\(open\|openat\|creat\)(.*\".*\"" | \grep -v -w "ENOENT"
 rm $TMPFILE
}

# Example: describe_unix_error 127
function describe_unix_error {
  if [[ $# = 0 ]]; then
    perl -le 'print $!+0, "\t", $!++ for 0..127'
  else
    local A="$@";
    perl -le 'print $!+0, "\t", $!++ for 0..127' | \grep -w "${A// /\\|}"
  fi
}

# Sort and compare with `meld':
function meld_sorted {
  [[ -f "$1" && -f "$2" ]] || { return 1; }
  local TMPFILE1=$(mktemp)
  local TMPFILE2=$(mktemp)
  sort "$1" | uniq > $TMPFILE1
  sort "$2" | uniq > $TMPFILE2
  meld --label="$1 vs $2" $TMPFILE1 $TMPFILE2
  rm -f $TMPFILE1 $TMPFILE2
}

# Usage: in_place [-f|--force] TARGET COMMAND [ARGS]..
# ---
# Turn a simple filter (stdin -> stdout) into a command that modify the TARGET file in place.
# The option -f forces writing even when the command (for instance `grep') fails.
# ---
# Example:
#   in_place some.log sort
#   in_place some.db  eval "sort | uniq"
# ---
function in_place {
  local FORCE
  # Force writing even when the command (for instance `grep') fails:
  if [[ $1 = "-f" || $1 = "--force" ]]; then
    FORCE=y
    shift
  fi
  # ---
  [[ $# -ge 2 ]] || { echo "Usage: in_place TARGET COMMAND [ARGS].." 1>&2; return 1; }
  # ---
  local TARGET=$1; shift
  local CMD=$1; shift
  [[ -f $TARGET ]] || { echo "ERROR: in_place: target file \`$TARGET' doesn't exist" 1>&2; return 2; }
  # ---
  local TMPFILE=$(mktemp)
  "$CMD" "$@" <$TARGET >$TMPFILE
  # ---
  local CODE=$?
  if [[ $CODE = 0 || $FORCE = y ]]; then
    cat $TMPFILE >$TARGET;
  else
    echo "WARNING: in_place: target file \`$TARGET' leaved unchanged" 1>&2;
  fi
  rm $TMPFILE
  return $CODE
}

# Usage: inherit_env PID [PATTERN]
# Example:
# inherit_env  1 'XDG\|DBUS\|ENV\|VNC_'
function inherit_env {
  local PID=$1
  local PATTERN="$2"
  # ---
  [[ -e /proc/$PID/environ ]] || return 1
  # ---
  local TMPFILE=$(mktemp)
  strings /proc/$PID/environ | grep "\($PATTERN\)[^=]*[=]" | sed -e "s@\(.*\)=\(.*\)@export \1='\2'@g" > $TMPFILE
  source $TMPFILE
  local CODE=$?
  rm $TMPFILE
  return $CODE
}
