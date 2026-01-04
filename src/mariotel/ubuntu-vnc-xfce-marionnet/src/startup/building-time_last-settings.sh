#!/bin/bash

# Usage: to be called by the Dockerfile at container building-time
# ---
# Purpose: set and adapt the common part of the filesystem
# (not the user related part, i.e. not /home sub-directories)

#-------------------------#
#          Env            #
#-------------------------#

set -e
set -x
# --
# Inherit a correct environment for the rest of script from parent
export $(strings /proc/$PPID/environ)
# --
export STARTUPDIR="${STARTUPDIR:-/dockerstartup}"
export BASH_ENV=$STARTUPDIR/bash_env.sh
# --

#-------------------------#
#        Timezone         #
#-------------------------#

echo "Europe/Paris" > /etc/timezone
ln -sf /usr/share/zoneinfo/Europe/Paris /etc/localtime
dpkg-reconfigure -f noninteractive tzdata

#-------------------------#
#   Call Xvnc as root     #
#-------------------------#

# The command used by vncserver to launch the Xvnc server should be:
#   ---
#   /usr/bin/sudo /usr/bin/Xvnc :$displayNumber
#   ---

TARGET="/usr/bin/vncserver"
cp -a $TARGET $TARGET.orig

PATCH="$STARTUPDIR/tigervnc-1.10.1.unix.vncserver.patch"
patch $TARGET $PATCH
rm -f $PATCH

#-------------------------#
# Prevent logout by XFCE  #
#-------------------------#

sed -i -e 's/Exec=.*/Exec=/' /usr/share/applications/xfce4-session-logout.desktop

#-------------------------#
#    Apache & PHP 7.2     #
#-------------------------#

a2enmod userdir
phpenmod pdo_mysql

# ---
TARGET="/etc/apache2/mods-enabled/php7.2.conf"
test -f $TARGET
gawk -i inplace '/^<IfModule mod_userdir.c>$/,/<^\/IfModule>$/ {print "#",$0; next} {print}' $TARGET

# ---
TARGET="/etc/php/7.2/apache2/php.ini"
test -f $TARGET
cat <<EOF >> $TARGET
display_errors = On
display_startup_errors = On
error_reporting = E_ALL
error_prepend_string = "<span style='color: #ff0000'>"
error_append_string = "</span>"
EOF

#-------------------------#
#     Optional tools      #
#-------------------------#

# The end of the Docker building process is the right time to install some optional tools.
# If we were to change our plans, only the last Docker layer would have to be rebuilt.

# --- gdebi
apt-get install -y gdebi-core gdebi

# --- R
apt-get install -y r-base r-base-dev r-cran-igraph
TARGET="rstudio-1.3.1093-amd64.deb"
wget -q "https://download1.rstudio.org/desktop/bionic/amd64/$TARGET" && { gdebi -n $TARGET; rm -f $TARGET; }




