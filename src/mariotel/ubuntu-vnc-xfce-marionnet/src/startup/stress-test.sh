#/bin/bash

# This script starts marionnet with a complex project, then starts all components,
# then sleeps some seconds, and finally stops all running components.
# J.V. Loddo

# Start Marionnet:
marionnet /dockerstartup/stress-test.mar &
sleep 3s

# Close warnings, if any:
for WID in $(xdotool search --onlyvisible --classname Marionnet); do
  xdotool windowfocus $WID
  xdotool key KP_Enter
done

# ---
sleep 1s

# Get the window identifier:
WID=$(xdotool search --onlyvisible --name Marionnet | tail -n 1)

# The tab key must be pressed several times (6) to join the button "Start all":
function goto_start_button {
  local SIX_TABS="Tab + Tab + Tab + Tab + Tab + Tab"
  xdotool windowfocus $WID
  xdotool key $SIX_TABS
}

# Start now all components:
goto_start_button
xdotool key KP_Enter

# Sleep a while:
sleep 20s

# Stop all components:
xdotool windowfocus $WID # focus
xdotool key Tab + Tab + KP_Enter # stop
sleep 1s
CONFIRM_WID=$(xdotool search --onlyvisible --classname Marionnet | tail -n 1)
xdotool windowfocus $CONFIRM_WID && xdotool key Tab + KP_Enter # confirm

# Leave the Marionnet's window in the desktop.
# Success.
