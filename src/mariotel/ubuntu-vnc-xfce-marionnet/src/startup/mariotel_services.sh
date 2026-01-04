#!/bin/bash

# Usage: to be called by `vnc_startup.sh' with `sudo' at container running-time

#-------------------------#
#          Env            #
#-------------------------#

set +e
# --
# Inherit a correct environment for the rest of script from process
# `vnc_startup.sh' (PID 1) called by student or teacher (not as root):
export $(strings /proc/1/environ)
# --
export STARTUPDIR="${STARTUPDIR:-/dockerstartup}"
export BASH_ENV=$STARTUPDIR/bash_env.sh
# --
[[ -n $USER    ]] || export USER=$(id -un)

#-------------------------#
#    Startup services     #
#-------------------------#

service marionnet-daemon start
