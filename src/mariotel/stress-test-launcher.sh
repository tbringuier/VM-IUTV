#!/bin/bash

# Usage:
# ./stress-test-launcher.sh SESSION_ID

SESSION_ID="$1"

[[ -n $SESSION_ID ]] || exit 1

set -x

for i in $(docker ps | grep "mariotel.id-${SESSION_ID}.student" | awk '{print $NF}'); do
  # docker exec --user 1001:1001 $i bash -c 'marionnet /dockerstartup/stress-test.mar &' &>/dev/null &
  docker exec --user 1001:1001 $i bash -c '/dockerstartup/stress-test.sh &' &>/dev/null &
done

