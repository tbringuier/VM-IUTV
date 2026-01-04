#!/bin/bash

# This file is part of Mariotel
# Copyright (C) 2020  Jean-Vincent Loddo
# Copyright (C) 2020  Université Sorbonne Paris Nord
# GPL v3

#-------------------------#
#          Env            #
#-------------------------#

set +e
# --
# Inherit a correct environment for the rest of script from process
# `vnc_startup.sh' (PID 1) called by student or teacher (not as root):
export $(strings /proc/1/environ)
# --
STARTUPDIR="${STARTUPDIR:-/dockerstartup}"
LOGFILE=${LOGFILE:-${STARTUPDIR}/no_vnc_startup.log}
[[ -e $LOGFILE ]] || exit 1

#-------------------------#
#     Read connections    #
#-------------------------#

# Example of result (on stdout):
# ---
# 172.17.0.1 2020-11-25 01:37:20
# 172.17.0.1 2020-11-26 00:08:33
# ---
awk '/WebSocket connection$/ {print $1,$4,$5}' $LOGFILE | tr '/' ' ' | tr -d '][' \
    | while read IP DATE; do
        # Ex: IP="172.17.0.1" DATE="25 Nov 2020 01:37:20"
        # Format date:
        DATE=$(date -d "$DATE" '+%Y-%m-%d %H:%M:%S')
        echo $IP $DATE
        done
# ---
