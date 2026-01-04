# This file is part of marionnet
# Copyright (C) 2020  Jean-Vincent Loddo
# Copyright (C) 2020  Université Sorbonne Paris Nord
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# =============================================================
#                     Dependencies
# =============================================================

# Debian:
REQUIRED_PACKAGES = libapache2-mod-php php-mysql php-mbstring mariadb-server

# Ubuntu 20.04:
# https://downloads.mariadb.org/mariadb/repositories/#distro=Ubuntu&distro_release=focal--ubuntu_focal&mirror=cnrs&version=10.5

# Source:
# https://www.digitalocean.com/community/tutorials/how-to-install-linux-apache-mysql-php-lamp-stack-on-ubuntu-20-04-fr
# ---
# Après l'installation de mysql-server, lancer :
# ---
# mysql_secure_installation
# ---
# => MEDIUM Length >= 8, numeric, mixed case, and special characters
# openssl passwd -6 -salt NzxHhFTF '*a****e**y*****2*!*'
# $6$NzxHhFTF$3AbzouvEARnquaS2JDc4NssG2JXPYzFkuK0xeMtKf9KxG9gucFTSbcdZr9ovZNr20Xkio1wcilnWc1xNt9/J91
# ---
# Source: https://www.cyberciti.biz/faq/how-to-install-adminer-on-ubuntu-20-04-lts/
# sudo systemctl stop   apache2.service
# sudo systemctl start  apache2.service
# sudo systemctl enable apache2.service
# ---
# sudo apt install default-mysql-server sqlite3 php-pear
# ---
# systemctl reload apache2

# ===============
# SOLUTION
# ===============
# https://stackoverflow.com/questions/42421585/default-password-of-mysql-in-ubuntu-server-16-04/49778695#49778695
# https://stackoverflow.com/questions/39281594/error-1698-28000-access-denied-for-user-rootlocalhost
# https://stackoverflow.com/questions/6445917/connect-failed-access-denied-for-user-rootlocalhost-using-password-yes
# ---
# sudo mysql --defaults-file=/etc/mysql/debian.cnf
# CREATE USER 'mariotel'@'localhost' IDENTIFIED BY 'MariotelMysql2020!!';
# GRANT ALL PRIVILEGES ON *.* TO 'mariotel'@'localhost';
# FLUSH PRIVILEGES;
# COMMIT;
# EXIT;
# sudo service mysql stop
# sudo service mysql start
# sudo mysql -u mariotel -p # ça marche !!!
# ---
# sudo mysql --defaults-file=/etc/mysql/debian.cnf <users-table/users.sql
# ---
# php -a
# Interactive mode enabled
#
# php > define('DB_SERVER', 'localhost');
# php > define('DB_USERNAME', 'mariotel');
# php > define('DB_PASSWORD', 'MariotelMysql2020!!');
# php > define('DB_NAME', 'mariotel');
# php > mysqli_connect(DB_SERVER, DB_USERNAME, DB_PASSWORD, DB_NAME);
# OK!

# ===============
# NE PAS FAIRE:
# ===============
# sudo mysql_secure_installation
#   Enter current password for root (enter for none): Just press the Enter
#   Set root password? [Y/n]: Y
#   New password: Enter password
#   Re-enter new password: Repeat password
#   Remove anonymous users? [Y/n]: Y
#   Disallow root login remotely? [Y/n]: Y
#   Remove test database and access to it? [Y/n]:  Y
#   Reload privilege tables now? [Y/n]:  Y
# ---
# sudo mysql
#   USE mysql;
#   UPDATE user SET plugin='' WHERE user ='root';
#   FLUSH PRIVILEGES;
#   EXIT;
# ---

dependencies:
	@echo "Required packages: $(REQUIRED_PACKAGES)"
	@which dpkg 1>/dev/null || { echo "Not a Debian system (oh my god!); please install packages corresponding to: $(REQUIRED_PACKAGES)"; exit 1; }
	@dpkg 1>/dev/null -l $(REQUIRED_PACKAGES) || \
		  sudo apt install -y $(REQUIRED_PACKAGES);
	@sudo systemctl enable apache2
	@sudo service apache2 restart
	@sudo systemctl enable mariadb.service
	@sudo service mysql restart
	@echo Ok.

# =============================================================
#                          install
# =============================================================

WEB_SERVER_DIR = /var/www/html
WEB_SERVER_DIR_IMAGES = $(WEB_SERVER_DIR)/images
IGNORE_OPTION = -ignore 'Name {Makefile,package.json,yarn.lock,README.sh,BACKUP*}'
# ---
UNISON_BINARY = unison-2.48.4

install-deprecated:
	# sudo cp *.php *.html *.css $(WEB_SERVER_DIR)
	sudo cp *.php $(WEB_SERVER_DIR)
	sudo mkdir -p $(WEB_SERVER_DIR_IMAGES)
	sudo cp images/*.png $(WEB_SERVER_DIR_IMAGES)/

install-with-unison:
	sudo ../UNISON/$(UNISON_BINARY) -batch -auto -silent $(IGNORE_OPTION) -force ./ ./ $(WEB_SERVER_DIR)




