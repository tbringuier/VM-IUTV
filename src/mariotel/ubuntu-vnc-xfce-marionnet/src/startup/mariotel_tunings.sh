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

# Usage: to be sourced by vnc_startup.sh or called with `docker exec'

#-------------------------#
#          Env            #
#-------------------------#

set +e
export STARTUPDIR="${STARTUPDIR:-/dockerstartup}"
export BASH_ENV=$STARTUPDIR/bash_env.sh

[[ -n $USER    ]] || export USER=$(id -un)
[[ -n $LOGNAME ]] || export LOGNAME=$USER

# ---
# Trace:
[[ -r ${BASH_ENV} ]] && { source ${BASH_ENV}; trace_bash_call; }

# Get a correct environment for the rest of script from process `xfce4-panel':
if pgrep xfce4-panel &>/dev/null; then
  export $(strings /proc/$(pgrep xfce4-panel | tail -n 1)/environ | grep "XDG\|DBUS\|ENV\|VNC_")
fi

#-------------------------#
#     Home ownership      #
#-------------------------#

# Should be one of:
# ---
#   chown -R student:student /home/student
#   chown -R teacher:teacher /home/teacher
# ---
chown -R $(id -un):$(id -gn) $HOME

#-------------------------#
#         Tools           #
#-------------------------#

# Example:
#   is_package_installed gedit-plugin-word-completion
# ---
function is_package_installed {
  dpkg -s "$1" 2>/dev/null | \grep -iq "^Status:.*installed"
}
export -f is_package_installed

# Usage: gsettings_help PATTERN
# ---
# Examples:
#   gsettings_help charmap
#   gsettings_help "gedit.*completion"
# ---
function gsettings_help {
  local PATTERN="$1"
  local SCHEMAS=$(gsettings list-schemas | grep -i "$PATTERN")
  local SCHEMA KEY
  local COLS=$((2*$(tput cols)/3))
  # ---
  for SCHEMA in $SCHEMAS; do
    echo "SCHEMA: $SCHEMA"
    for KEY in $(gsettings list-keys $SCHEMA); do
      echo -e "--- KEY: $KEY"
      echo -e "    EXAMPLE: gsettings set $SCHEMA $KEY VALUE\n    ---"
      gsettings describe $SCHEMA $KEY | fmt -w $COLS | awk '{print "    "$0}'
      echo
    done
  done
}
export -f gsettings_help

#-------------------------#
#       marionnet         #
#-------------------------#

TARGET=$HOME/Desktop/marionnet.desktop
if [[ -w $TARGET ]]; then
  sed -i -e "s@Path=.*@Path=$HOME@" $TARGET
fi

# thunar: .mar -> marionnet
# ---
# Thanks to Franck Butelle for solving this issue.
# ---
TARGET=${STARTUPDIR}/marionnet-mar.xml
if type -p xdg-mime &>/dev/null && test -r $TARGET; then
  xdg-mime install --mode user $TARGET
fi
rm -f $HOME/.local/share/mime/packages/marionnet-mar.xml
# ---
TARGET="$HOME/.config/mimeapps.list"
touch $TARGET
if ! grep -q 'application/x-marionnet=marionnet.desktop;' $TARGET; then
cat <<EOF >> $TARGET
[Added Associations]
application/x-marionnet=marionnet.desktop;
application/gzip=xarchiver.desktop;
EOF
fi
# ---
TARGET=$HOME/.local/share/applications
mkdir -p $TARGET
ln -sf $HOME/Desktop/marionnet.desktop $TARGET/marionnet.desktop
# ---
TARGET=$HOME/.local/share/mime/packages/marionnet-mar.xml
if [[ ! -e $TARGET ]]; then
cat <<EOF >$TARGET
<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
    <mime-type type="application/x-marionnet">
        <comment>marionnet file</comment>
        <glob-deleteall/>
        <glob pattern="*.mar"/>
    </mime-type>
</mime-info>
EOF
fi
# ---
update-mime-database $HOME/.local/share/mime

#-------------------------#
#        apache           #
#-------------------------#

# sudo a2enmod userdir
if a2query -m userdir &>/dev/null; then
  mkdir -p $HOME/public_html
  chmod o+rx $HOME/public_html
fi

#-------------------------#
#         gedit           #
#-------------------------#

# ---
GEDIT_ACTIVE_PLUGINS="['terminal', 'findinfiles', 'codecomment', 'charmap', 'bookmarks', 'wordcompletion', 'quickopen', 'snippets']"
gsettings set org.gnome.gedit.plugins active-plugins "$GEDIT_ACTIVE_PLUGINS"
# ---
# ii  gedit-plugin-bookmarks            3.28.1-1      amd64      Bookmarks plugin for gedit
# ii  gedit-plugin-character-map        3.28.1-1      amd64      Character Map plugin for gedit
# ii  gedit-plugin-code-comment         3.28.1-1      amd64      Code Comment plugin for gedit
# ii  gedit-plugin-terminal             3.28.1-1      amd64      Terminal plugin for gedit
# ii  gedit-plugin-word-completion      3.28.1-1      amd64      Word Completion plugin for gedit

# ---
# To see other useful options:
# gsettings list-recursively | grep -i gedit
# ---
if type -p gsettings gedit &>/dev/null; then
gsettings set org.gnome.gedit.preferences.editor display-right-margin true
gsettings set org.gnome.gedit.preferences.editor highlight-current-line true
gsettings set org.gnome.gedit.preferences.editor bracket-matching true
gsettings set org.gnome.gedit.preferences.editor display-line-numbers true
gsettings set org.gnome.gedit.preferences.editor editor-font 'Monospace 12'
gsettings set org.gnome.gedit.preferences.editor tabs-size 2
gsettings set org.gnome.gedit.preferences.editor use-default-font false
gsettings set org.gnome.gedit.preferences.editor insert-spaces true
gsettings set org.gnome.gedit.preferences.editor auto-indent true
gsettings set org.gnome.gedit.preferences.editor auto-save true
gsettings set org.gnome.gedit.preferences.editor auto-save-interval 2
# ---
if is_package_installed gedit-plugin-word-completion; then
  gsettings set org.gnome.gedit.plugins.wordcompletion interactive-completion true
  gsettings set org.gnome.gedit.plugins.wordcompletion minimum-word-size 2
fi
# ---
if is_package_installed gedit-plugin-terminal; then
  # gsettings set org.gnome.gedit.preferences.ui bottom-panel-visible true
  : # do nothing, it's too cumbersome an option
else
  gsettings set org.gnome.gedit.preferences.ui bottom-panel-visible false
fi
# ---
fi

#-------------------------#
#         emacs           #
#-------------------------#

# Thanks to Luca Saiu for this section.
# ---
TARGET=$HOME/.emacs
if type -p emacs &>/dev/null; then
if test ! -e $TARGET; then
cat <<"EOF" >$TARGET
;; ---
(set-language-environment "UTF-8")
(setq inhibit-splash-screen t)
(setq truncate-partial-width-windows nil)
(setq ring-bell-function 'ignore)
(display-time-mode t)
(setq-default indent-tabs-mode nil)
(setq abbrev-file-name "~/.emacs.d/abbrev_defs")
;; ---
(setq scalable-fonts-allowed t)
(global-font-lock-mode t)
;; ---
(column-number-mode t)
(transient-mark-mode t)
(show-paren-mode t)
(setq backup-by-copying t)
;; ---
;; major modes
(dolist (h '(emacs-lisp-mode-hook lisp-mode-hook c-mode-hook
             sh-mode-hook asm-mode-hook
             autoconf-mode-hook m4-mode-hook
             makefile-mode-hook text-mode-hook
             conf-unix-mode-hook
             tuareg-mode-hook caml-mode-hook
             scheme-mode-hook python-mode-hook))
  (add-hook h 'linum-mode))
;; ---
;; GC:
(setq gc-cons-threshold 100000)
;; ---
(setq history-length 40)
(dolist (thing '(minibuffer-history evil-ex-history kill-ring))
  (put thing 'history-length history-length))
;; ---
;; Copy and paste by C-c and C-v:
;; (cua-mode t)
;; ---
;; Trailing spaces:
(setq-default indicate-empty-lines t
              show-trailing-whitespace t)
;; ---
;; Ispell support, particularly for writing in French:
(setq ispell-program-name "ispell")
;; ---
;;(ispell-change-dictionary "francais")
;; ---
(setq ispell-lazy-highlight t)
(setq ispell-highlight-p t)
(setq ispell-highlight-face 'secondary-selection)
(setq lazy-highlight-interval 0.01)
(setq ispell-highlight-face 'region)
;; ---
;; Manually: flyspell-mode
;; ---
EOF
fi # test
# ---
mkdir -p $HOME/.emacs.d
touch $HOME/.emacs.d/abbrev_defs
fi # type

#-------------------------#
#        gucharmap        #
#-------------------------#

# gsettings list-schemas | grep -i charmap
# gsettings list-keys org.gnome.Charmap
# gsettings describe org.gnome.Charmap font
# ---
# gsettings list-recursively | grep -i charmap
if type -p gsettings gucharmap &>/dev/null; then
# last-char 96 is backquote => script "Common"
gsettings set org.gnome.Charmap last-char 96
gsettings set org.gnome.Charmap group-by 'script'
gsettings set org.gnome.Charmap snap-cols-pow2 false
fi

#-------------------------#
#      xfce desktop       #
#-------------------------#

# xfconf-query --channel xfce4-desktop --list
XFCE_BLUE="/usr/share/backgrounds/xfce/xfce-blue.jpg"
PROPERTY="/backdrop/screen0/monitor0/workspace0/last-image"
if type -p xfconf-query &>/dev/null && test -f $XFCE_BLUE; then
# xfconf-query --channel xfce4-desktop --property $PROPERTY --set $XFCE_BLUE
xfconf-query --channel xfce4-desktop --property $PROPERTY -n -t string -s $XFCE_BLUE
fi

# Transparent background for text under desktop icons
TARGET=$HOME/.gtkrc-2.0
if test ! -f $TARGET; then
cat <<EOF >>$TARGET
style "xfdesktop-icon-view" {
XfdesktopIconView::label-alpha = 0
}
widget_class "*XfdesktopIconView*" style "xfdesktop-icon-view"
EOF
fi

# xfconf-query -c xsettings --list
xfconf-query -c xsettings -p "/Net/IconThemeName" -s "Humanity"
xfconf-query -c thunar    -p "/last-window-maximized" -s false

#-------------------------#
#         genmon          #
#-------------------------#

# Too many threads launched for nothing (why?).
# Should be fixed at every container restart:
GENMON_THREADS=$(pgrep -w 'panel-.*genmon' | wc -l)
if [[ $GENMON_THREADS -gt 3 ]]; then
  kill $(pgrep -w 'panel-.*genmon' | tail -n 1)
fi

#-------------------------#
#  student name as title  #
#-------------------------#

# --- TODO:
# /usr/share/usr/local/share/noVNCdim/app/ui.js
# ---
