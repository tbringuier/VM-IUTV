# This file is part of Mariotel
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

REQUIRED_PACKAGES = apt-transport-https ca-certificates curl gnupg-agent software-properties-common docker-compose php libapache2-mod-php

dependencies:
	@echo "Required packages: $(REQUIRED_PACKAGES)"
	@which dpkg 1>/dev/null || { echo "Not a Debian system (oh my god!); please install packages corresponding to: $(REQUIRED_PACKAGES)"; exit 1; }
	@dpkg 1>/dev/null -l $(REQUIRED_PACKAGES) || \
		  sudo apt install -y $(REQUIRED_PACKAGES);
	@sudo usermod -aG docker $$USER && \
	echo "Warning: disconnect and reconnect yourself to be able to run docker commands!";
	@echo Ok.

# =============================================================
#                  git clone accetto repository
# =============================================================

# This section remains here to document the initial building
# of ubuntu-vnc-xfce-marionnet/ from the accetto's project

SOURCE=ubuntu-vnc-xfce
TARGET=ubuntu-vnc-xfce-marionnet
SOURCE_URL=https://github.com/accetto/ubuntu-vnc-xfce.git
# ---
clone:
	test -d $(SOURCE) || git clone $(SOURCE_URL)
	test -d $(TARGET) || cp -a $(SOURCE) $(TARGET)
	find $(TARGET)/utils/ -name "*.sh" -exec chmod +x {} \;
	chmod +x $(TARGET)/hooks/build


# =============================================================
#                          edit
# =============================================================

# ---
TARGET1=ubuntu-vnc-xfce-marionnet
EXCLUDE_FROM_EDITING1=
# ---
TARGET2=webgui
EXCLUDE_FROM_EDITING2= */node_modules/*

edit:
	test -n "$$EDITOR" && \
	  eval $$EDITOR Makefile \
	    $$(find $(TARGET1) \( -name ".git*" -o -name "LICENSE" -o -name "$(EXCLUDE_FROM_EDITING1)" -o -name "*.jpg" -o -name "*.png" \) -prune -o -type f -print) \
	    $$(find $(TARGET2) \( -wholename "$(EXCLUDE_FROM_EDITING2)" -o -name "*.jpg" -o -name "*.png" \) -prune -o -type f -print) &


# =============================================================
#                       docker build
# =============================================================

OPTION_ARG_HOME = --build-arg ARG_HOME=/home/student
# ---
# Adapt certificate's meta-informations to your organization:
OPTION_ARG_SSL_SUBJECT= --build-arg ARG_SSL_SUBJECT='/C=FR/ST=Seine-Saint-Denis/L=Villetaneuse/O=Universite Sorbonne Paris Nord (USPN)/CN=*.marionnet.org'
# ---
# Comment this line if you want to enable also http connections by default:
# # # # OPTION_ARG_NO_VNC_SSLONLY = --build-arg ARG_NO_VNC_SSLONLY='--ssl-only'
# ---
VERSION = 0.65
# ---
build:
	docker build -t "mariotel:$(VERSION)" $(OPTION_ARG_HOME) $(OPTION_ARG_SSL_SUBJECT) $(OPTION_ARG_NO_VNC_SSLONLY) $(TARGET)

# Mr proper:
remove-all-untagged-images:
	docker rmi $$(docker images -a | awk '$$1=="<none>" && $$2=="<none>" {print $$3}')


# =============================================================
#               Support for targets with arguments
# =============================================================
# Thanks to:
#   https://stackoverflow.com/questions/2214575/passing-arguments-to-make-run/14061796#14061796
#   https://medium.com/lebouchondigital/passer-des-arguments-%C3%A0-une-target-gnu-make-1ddab618c32f

SUPPORTED_COMMANDS := edit-file
SUPPORTS_MAKE_ARGS := $(findstring $(firstword $(MAKECMDGOALS)), $(SUPPORTED_COMMANDS))
ifneq "$(SUPPORTS_MAKE_ARGS)" ""
  COMMAND_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(COMMAND_ARGS):;@:)
endif


# =============================================================
#                    run (testing/debugging)
# =============================================================

# Source: https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities
# Examples: --cap-add=ALL --cap-drop=MKNOD
# Needed by marionnet-daemon to be able to create tun/tap interfaces (just --cap-add=NET_ADMIN):
OPTION_CAP = --cap-add=ALL
# OPTION_CAP = --privileged=true

# Needed by UML:
# OPTION_SHM = --tmpfs /dev/shm:rw,nosuid,nodev,exec,relatime,size=65536k
# OPTION_SHM = --tmpfs /dev/shm:rw,nosuid,nodev,exec,relatime,size=1G
OPTION_SHM = --tmpfs /dev/shm:rw,nosuid,nodev,exec,relatime,size=256M

# Share marionnet's filesystems installed on the docker's host:
# OPTION_SHARE = --read-only -v /usr/local/share/marionnet:/usr/local/share/marionnet # NON FUNZIONA
# OPTION_SHARE = -v /usr/local/share/marionnet:/usr/local/share/marionnet:ro
OPTION_SHARE = --mount type=bind,src=/usr/local/share/marionnet,dst=/usr/local/share/marionnet,readonly

# Source: https://docs.docker.com/storage/tmpfs/
# /tmp must support sparse files:
# OPTION_TMP = --mount type=tmpfs,destination=/tmp
# OPTION_TMP = --mount type=bind,source=$(BASE_TMP),target=/tmp
# OPTION_TMP = -v $(BASE_TMP):/tmp     # CREA GROSSI PROBLEMI
# OPTION_TMP = --mount type=tmpfs,destination=/tmp,tmpfs-mode=1770,tmpfs-size=268435456
# OPTION_TMP='--tmpfs /tmp:rw,exec,size=393216k,mode=1770'

# 8G og disk space per container:
# OPTION_STORAGE= --storage-opt size=8G
# --storage-opt is supported only for overlay over xfs with 'pquota' mount option

# 1001:1001 is the uid:gid of the user `student':
OPTION_USER_STUDENT = --user 1001:1001
OPTION_PASSWORD_STUDENT = -e VNC_PW=student

# 1002:1002 is the uid:gid of the user `teacher':
OPTION_USER_TEACHER = --user 1002:1002
OPTION_PASSWORD_TEACHER = -e VNC_PW=teacher

# OPTION_RM = --rm
OPTION_RM =

# OPTION_RESTART = --restart=always
OPTION_RESTART =

# 10m + 3h + 15m = 12300s
# OPTION_STOP_TIMEOUT = --stop-timeout 12300
OPTION_STOP_TIMEOUT =

BASE_TMP = /tmp-mariotel
OPTION_HOSTNAME = --hostname ws01

# Other interesting docker options:
# --workdir /home/student --entrypoint /dockerstartup/vnc_startup.sh

# For testing/debugging with the local docker:
run-as-student:
	NO_VNC_SSLONLY="" docker run -d --name mariotel $(OPTION_SHM) -p 26901:6901 $(OPTION_CAP) $(OPTION_USER_STUDENT) $(OPTION_PASSWORD_STUDENT) $(OPTION_HOSTNAME) $(OPTION_TMP) $(OPTION_STORAGE) $(EXTRA_ARGS) mariotel:$(VERSION)
	echo "student:student" | docker exec --user 0:0 -i mariotel chpasswd

# For testing/debugging with the local docker:
run-as-teacher:
	docker run -d --name mariotel $(OPTION_SHM) -p 26901:6901 $(OPTION_CAP) $(OPTION_USER_TEACHER) $(OPTION_PASSWORD_TEACHER) $(OPTION_HOSTNAME) $(OPTION_TMP) $(OPTION_STORAGE) $(EXTRA_ARGS) mariotel:$(VERSION)
	echo "teacher:teacher" | docker exec --user 0:0 -i mariotel chpasswd

run-as-student-entrypoint-bash:
	EXTRA_ARGS="-t --entrypoint /bin/bash" make run-as-student

run-as-teacher-entrypoint-bash:
	EXTRA_ARGS="-t --entrypoint /bin/bash" make run-as-teacher

# Lazy:
CONTAINER = $(shell docker ps -a | grep mariotel:$(VERSION) | awk '{print $$1; exit(0);}')

restart:
	CONTAINER=$(CONTAINER); \
	docker restart $(CONTAINER)

# Edit a file in the running container from the host. Example:
#   make edit-file /usr/bin/vncserver
# ---
edit-file:
	CONTAINER=$(CONTAINER); \
	docker cp $$CONTAINER:$(COMMAND_ARGS) /tmp/; \
	test -n "$$EDITOR" && \
	eval $$EDITOR /tmp/$(shell basename $(COMMAND_ARGS)) && \
	docker cp /tmp/$(shell basename $(COMMAND_ARGS)) $$CONTAINER:$(COMMAND_ARGS)

# No -it here:
marionnet-daemon-status:
	docker exec --user 0:0 mariotel /etc/init.d/marionnet-daemon status

# No -it here:
marionnet-daemon-start:
	docker exec --user 0:0 mariotel /etc/init.d/marionnet-daemon start

prevent-xfce4-session-logout:
	docker exec --user 0:0 mariotel sed -ie 's/Exec=.*/Exec=/' /usr/share/applications/xfce4-session-logout.desktop

# For testing/debugging with the local docker:
shell-as-root:
	docker exec --user 0:0 -it mariotel /bin/bash

# For testing/debugging with the local docker:
shell-as-student:
	docker exec $(OPTION_USER_STUDENT) -it mariotel /bin/bash

# For testing/debugging with the local docker:
shell-as-teacher:
	docker exec $(OPTION_USER_TEACHER) -it mariotel /bin/bash

# TO BE ADAPTED:
BROWSER = chromium-browser --new-window
# ---
test-with-browser:
	$(BROWSER) 'http://127.0.0.1:26901/vnc.html?password=student'

# For testing/debugging with the local docker:
run-and-test: run-as-student test-with-browser


# =============================================================
#                       Control
# =============================================================

CURRENT_DATETIME = $(shell date +%Y-%m-%d.%H\h%M)
LAST_RUNNING_CONTAINER = $(shell docker ps -l | tail -n 1 | awk '{print $$1}')
LAST_RUNNING_IMAGE = $(shell docker ps -l | tail -n 1 | awk '{print $$2}' | tr ':' '-')

docker-commit:
	docker commit -p $(LAST_RUNNING_CONTAINER) $(LAST_RUNNING_IMAGE).$(CURRENT_DATETIME)

# Remove all running/stopped containers:
killall-containers:
	docker rm -f $(shell docker ps -aq)


# =============================================================
#                 Synchronize & install
#                  to a Mariotel server
# =============================================================

# TO BE ADAPTED:
SERVER_IP = 194.254.163.32
# --
SERVER_REPO = /home/mariotel/repo/
WEB_SERVER_REPO = /home/mariotel/repo/webgui
# ---
UNISON_BINARY = unison-2.48.4
UNISON_GTK_BINARY = unison-2.48.4-gtk
SERVER_COMMAND_OPTION = -servercmd $(SERVER_REPO)/UNISON/$(UNISON_BINARY)

# Install the same version of unison installed here on the server:
install-unison-on-server:
	scp ./UNISON/$(UNISON_BINARY) root@$(SERVER_IP):$(SERVER_REPO)/UNISON/

sync-with-unison-gtk:
	./UNISON/$(UNISON_GTK_BINARY) $(SERVER_COMMAND_OPTION) ./ ssh://root@$(SERVER_IP)/$(SERVER_REPO)
	ssh root@$(SERVER_IP) make -C $(WEB_SERVER_REPO) install-with-unison


sync-with-unison:
	./UNISON/$(UNISON_BINARY) -batch -auto -silent $(SERVER_COMMAND_OPTION) ./ ssh://root@$(SERVER_IP)/$(SERVER_REPO)
	ssh root@$(SERVER_IP) make -C $(WEB_SERVER_REPO) install-with-unison


