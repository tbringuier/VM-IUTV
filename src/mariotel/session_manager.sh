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

# Thanks for the assistance to:
# ---
# Xavier Monnin, Université Sorbonne Paris Nord
# ---

# Useful tool used to write this script:
# ---
# function flat_inspect () {
#  awk '{ for (i=1; i<=NF; i++) printf("Field#%02d: %s\n",i,$i); exit(0);}' "$@";
# }
# ---

# -------------------------------
#         Basic tools
# -------------------------------

# Usage: if SOURCEING; then echo "SOURCEING"; else echo "NOT SOURCEING"; fi
_SOURCEING_X="$0"; _SOURCEING_Y="$BASH_SOURCE"; function SOURCEING { [[ ! ${_SOURCEING_X} = ${_SOURCEING_Y} ]]; }

SOURCEING || set -x

function test_memory_and_swap_limits_support {
  test $(docker info --format="{{.MemoryLimit}}:{{.SwapLimit}}") = "true:true"
}

function test_xfs_quota_support {
  local DOCKER_ROOT_DIR=$(docker info --format="{{.DockerRootDir}}")
  # ---
  grep "$(df $DOCKER_ROOT_DIR -t xfs --output='source' 2>/dev/null | tail -n 1)" /proc/mounts | grep -w -q 'usrquota,prjquota'
  # ---
}

# Am I the only instance that runs?
# Usage: if am_I_the_only_running_instance; then echo TRUE; else echo FALSE; fi
function am_I_the_only_running_instance {
  local TMPFILE=$(mktemp 2>/dev/null || echo "/tmp/tmp.test-uri.$RANDOM")
  pgrep -x $(</proc/$$/comm) > $TMPFILE
  local y=$(wc -l <$TMPFILE)
  rm -f $TMPFILE
  [[ $y = 1 ]]
}

# May be "created", "restarting", "running", "removing", "paused", "exited", or "dead".
function docker_get_status {
  local CONTAINER="$1"
  docker inspect --format='{{.State.Status}}' $CONTAINER 2>/dev/null
}


# -------------------------------
#         Global vars
# -------------------------------

# Source: https://docs.docker.com/engine/reference/run/#runtime-privilege-and-linux-capabilities
# Examples: --cap-add=ALL --cap-drop=MKNOD
# Needed by marionnet-daemon to be able to create tun/tap interfaces (just --cap-add=NET_ADMIN):
OPTION_CAP='--cap-add=ALL'
# OPTION_CAP="--privileged=true"

# Needed by UML (320M reserved for shared memory):
OPTION_SHM='--tmpfs /dev/shm:rw,nosuid,nodev,exec,relatime,size=320M'

# tmpfs-size is 384M (384*1024 = 393216)
# OPTION_TMP='--tmpfs /tmp:rw,exec,size=393216k,mode=1770'
# Disabled: from image version 0.60 we use the root filesystem (overlay) as usual

# 1G RAM + 2G swap (total 3G per container, if supported)
unset OPTION_MEMORY_LIMITS
test_memory_and_swap_limits_support && OPTION_MEMORY_LIMITS='--memory="1g" --memory-swap="3g"'

# 1G disk space available per container (if supported):
unset OPTION_DISK_LIMITS
test_xfs_quota_support && OPTION_DISK_LIMITS='--storage-opt size=1G'

# No more than 4 cores for a container:
OPTION_CPU_LIMITS='--cpus="4.0"'

# 1001:1001 is the uid:gid of the user `student':
OPTION_USER_STUDENT='--user 1001:1001'
# Will be redefined according to the session's ID:
PASSWORD_STUDENT='student'
OPTION_PASSWORD_STUDENT="-e VNC_PW=${PASSWORD_STUDENT}"

# 1002:1002 is the uid:gid of the user `teacher':
OPTION_USER_TEACHER="--user 1002:1002"
# Will be redefined according to the session's ID:
PASSWORD_TEACHER='nastyvirus19'
OPTION_PASSWORD_TEACHER="-e VNC_PW=${PASSWORD_TEACHER}"

# Will be redefined according to the student index:
STUDENT_INDEX=1
OPTION_HOSTNAME="--hostname ws$(printf "%02d" $STUDENT_INDEX)"

# To be adapted according to the actual mysql (or maria-db) installation:
PASSWORD_MYSQL='MariotelMysql2020!!'

IMAGE_VERSION=$(docker images | awk '$1=="mariotel"' | head -n 1 | awk '{print $1":"$2}')
IMAGE_VERSION=${IMAGE_VERSION:-"mariotel:0.65"}
# IMAGE_VERSION="mariotel:0.5" # temporary

DATABASE='mariotel'
TABLE_sessions="$DATABASE.sessions"

# Must be consistent with the Dockerfile:
NO_VNC_HOME="/usr/share/usr/local/share/noVNCdim"
NO_VNC_LOCAL_PORT=6901
# Defined in config.php:
# $NO_VNC_STARTING_PORT=26900

# -------------------------------
#        SSL web proxy
# -------------------------------
# ---
# Thanks to Nicolas Greneche (USPN) who gave me the necessary information to write this section.
# ---
# Sources:
#   https://connect.ed-diamond.com/Linux-Pratique/LP-123/Les-certificats-de-l-emission-a-la-revocation
#   https://stackoverflow.com/questions/30977264/subject-alternative-name-not-present-in-certificate
# ---
# SSL specific global variables (to be adapted):
MARIOTEL_SITE_DNS1="tel.marionnet.org"
# ---
MARIOTEL_SITE_PUBLIC_CERT="/etc/ssl/certs/mariotelserver.crt"
MARIOTEL_SITE_PRIVATE_KEY="/etc/ssl/private/mariotelserver.key"
MARIOTEL_SITE_SSL_SUBJECT="/C=FR/ST=Seine-Saint-Denis/L=Villetaneuse/OU=USPN/O=Universite Sorbonne Paris Nord/CN=${MARIOTEL_SITE_DNS1}"
# ---
MARIOTEL_SITE_SSL_CHAIN_CONFIG="/etc/ssl/mariotelchain.config.ssl"
MARIOTEL_SITE_APACHE_CONF="/etc/apache2/sites-available/000-default.conf"
# ---
if [[ ! -r $MARIOTEL_SITE_PUBLIC_CERT || ! -r $MARIOTEL_SITE_PRIVATE_KEY ]]; then
  AWK_RANGE='/[:blank:]*<VirtualHost.*:443>/,/[:blank:]*<[/]VirtualHost>/'
  MARIOTEL_SITE_PUBLIC_CERT=$(awk "$AWK_RANGE" $MARIOTEL_SITE_APACHE_CONF | awk '$1 == "SSLCertificateFile"    {print $2}')
  MARIOTEL_SITE_PRIVATE_KEY=$(awk "$AWK_RANGE" $MARIOTEL_SITE_APACHE_CONF | awk '$1 == "SSLCertificateKeyFile" {print $2}')
fi

# ---
function install_a_self_signed_certificate {
  # ---
  if [[ ! -r $MARIOTEL_SITE_PUBLIC_CERT || ! -r $MARIOTEL_SITE_PRIVATE_KEY ]]; then
    return 1
  fi
  # --- Abbreviate names:
  local    SSL_SUBJECT="$MARIOTEL_SITE_SSL_SUBJECT"
  local CA_PUBLIC_CERT="$MARIOTEL_SITE_PUBLIC_CERT"
  local CA_PRIVATE_KEY="$MARIOTEL_SITE_PRIVATE_KEY"
  local CA_REQUEST_CSR="$MARIOTEL_SITE_REQUEST_CSR"
  local CA_PUB_SS_CERT="$MARIOTEL_SITE_SELF_SIGNED_CERT"
  # ---
  local addext_1="-addext basicConstraints=critical,CA:TRUE"
  local addext_2="-addext subjectAltName=DNS:${MARIOTEL_SITE_DNS1}"
  # ---
  # Create your CA self-signed certificate, if needed:
  if [[ ! -e $CA_PUB_SS_CERT ]]; then
    openssl req -new -key $CA_PRIVATE_KEY -subj "${SSL_SUBJECT}" ${addext_1} ${addext_2} -out $CA_REQUEST_CSR
    openssl x509 -signkey $CA_PRIVATE_KEY -days 365 -req -in $CA_REQUEST_CSR -out $CA_PUB_SS_CERT
  fi
}
# ---
MARIOTEL_SITE_REQUEST_CSR="/etc/ssl/certs/mariotelserver.csr"
MARIOTEL_SITE_SELF_SIGNED_CERT="/etc/ssl/certs/mariotelserver-self-signed.crt"
if [[ ! -r $MARIOTEL_SITE_REQUEST_CSR || ! -r $MARIOTEL_SITE_SELF_SIGNED_CERT ]]; then
  install_a_self_signed_certificate
fi
# ---
# mkdir -p ./webgui/certs
# ln -s ../../../etc/ssl/certs/mariotelserver-self-signed.crt webgui/certs/mariotel-autorite-USPN.crt
# ---

# ---
# When a container related certificate is signed with the mariotel's private key,
# the "subjectAltName" field is correctly injected by `openssl x509' using this
# configuration file (see variable `addext_2'):
# ---
if [[ ! -r $MARIOTEL_SITE_SSL_CHAIN_CONFIG ]]; then
cat > $MARIOTEL_SITE_SSL_CHAIN_CONFIG <<EOF
[ req ]
default_bits = 4096
prompt = no
distinguished_name = dn
req_extensions = req_ext
x509_extensions = v3_ca  # The extensions to add to the self signed cert

[ dn ]
$(sed <<<"${MARIOTEL_SITE_SSL_SUBJECT}" -e 's@/@\n@g' -e 's@=@ = @g' | tac | grep -v '^$')

[ req_ext ]
basicConstraints = critical,CA:TRUE
subjectAltName = DNS: ${MARIOTEL_SITE_DNS1}

[ v3_ca ]
# Extensions for a typical CA
# PKIX recommendation.
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid:always,issuer
basicConstraints = critical,CA:TRUE

[v3_req]
subjectAltName = @alt_names
[alt_names]
DNS.1 = ${MARIOTEL_SITE_DNS1}

[ proxy_cert_ext ]
# These extensions should be added when creating a proxy certificate
basicConstraints=CA:FALSE
nsComment = "OpenSSL Generated Certificate"
# PKIX recommendations harmless if included in all certificates.
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid,issuer
# This really needs to be in place for it to be a proxy certificate.
proxyCertInfo=critical,language:id-ppl-anyLanguage,pathlen:3,policy:foo
EOF
fi
# ---

# ---
# Usage:
#   provide_a_one_day_valid_certificate CONTAINER_NAME
# ---
function provide_a_one_day_valid_certificate {
  # ---
  if [[ ! -r $MARIOTEL_SITE_PUBLIC_CERT || ! -r $MARIOTEL_SITE_PRIVATE_KEY ]]; then
    return 1
  fi
  # ---
  local WORKDIR_BASE="/var/opt/mariotel/ssl"
  mkdir -p $WORKDIR_BASE
  # ---
  local CONTAINER_NAME=${1:-"mariotel.id-4234.student-16"}
  # ---
  local WORKDIR=$(mktemp -p ${WORKDIR_BASE} -d $CONTAINER_NAME.XXXXXX)
  # ---
  local CONTAINER_KEY=$WORKDIR/self-key.pem
  local CONTAINER_CSR=$WORKDIR/self.csr
  local CONTAINER_CRT=$WORKDIR/self-cert.pem
  local RESULT=$WORKDIR/self.pem
  # --- Abbreviate names:
  local    SSL_SUBJECT="$MARIOTEL_SITE_SSL_SUBJECT"
  local CA_PUBLIC_CERT="$MARIOTEL_SITE_PUBLIC_CERT"
  local CA_PRIVATE_KEY="$MARIOTEL_SITE_PRIVATE_KEY"
  local CA_REQUEST_CSR="$MARIOTEL_SITE_REQUEST_CSR"
  local CA_PUB_SS_CERT="$MARIOTEL_SITE_SELF_SIGNED_CERT"
  # ---
  local addext_1="-addext basicConstraints=critical,CA:TRUE"
  local addext_2="-extfile $MARIOTEL_SITE_SSL_CHAIN_CONFIG -extensions v3_req"
  # ---
  # Generate a fresh private key for the container:
  openssl genrsa -out $CONTAINER_KEY 4096 || return 2
  # ---
  # Generate the CSR form:
  openssl req -new -key $CONTAINER_KEY -subj "${SSL_SUBJECT}" ${addext_1} -out $CONTAINER_CSR || return 3
  # ---
  # Sign with the mariotel's private key:
  openssl x509 -req -days 1 -in $CONTAINER_CSR -CA $CA_PUB_SS_CERT -CAkey $CA_PRIVATE_KEY -CAcreateserial ${addext_2} -out $CONTAINER_CRT || return 4
  # ---
  # --- Merge the key with the certificate in a single file:
  cat $CONTAINER_KEY $CONTAINER_CRT > $RESULT
  md5sum $RESULT 1>&2
  # ---
  docker cp $RESULT $CONTAINER_NAME:$NO_VNC_HOME/ || return 5
  # ---
  # Mr proper:
  rm -f $WORKDIR/*
  rmdir $WORKDIR
}

# -------------------------------
#            starter
# -------------------------------

function mysql_cmd {
  mysql -s -u "mariotel" -p --password="$PASSWORD_MYSQL"
}

# ###
# mysql> DESCRIBE mariotel.sessions ;
# +--------------+--------------+------+-----+-------------------+-------------------+
# | Field        | Type         | Null | Key | Default           | Extra             |
# +--------------+--------------+------+-----+-------------------+-------------------+
# | id           | int          | NO   | PRI | NULL              | auto_increment    |
# | starts       | datetime     | NO   |     | NULL              |                   |
# | finish       | datetime     | NO   |     | NULL              |                   |
# | duration     | int          | NO   |     | NULL              |                   |
# | link         | varchar(255) | NO   | UNI | NULL              |                   |
# | status       | varchar(50)  | NO   |     | NULL              |                   |
# | username     | varchar(50)  | NO   | MUL | NULL              |                   |
# | email        | varchar(255) | YES  |     | NULL              |                   |
# | student_nb   | int          | NO   |     | NULL              |                   |
# | student_list | text         | YES  |     | NULL              |                   |
# | first_port   | int          | YES  |     | NULL              |                   |
# | created_at   | datetime     | YES  |     | CURRENT_TIMESTAMP | DEFAULT_GENERATED |
# +--------------+--------------+------+-----+-------------------+-------------------+

function starter {
# ---
local EXPECTED_STATUS=${1:-"planned"}
# ---
local CURRENT_DATE QUERY_TO_BE_STARTED QUERY_UPDATE
# ---
CURRENT_DATE=$(date '+%Y-%m-%d %H:%M:%S')
QUERY_TO_BE_STARTED="select  id, link, student_nb, first_port  from $TABLE_sessions where status = '$EXPECTED_STATUS' && '$CURRENT_DATE' >= DATE_ADD(starts, INTERVAL -5 MINUTE) && '$CURRENT_DATE' <= DATE_ADD(finish, INTERVAL +10 MINUTE);"
# ---
mysql_cmd <<<"$QUERY_TO_BE_STARTED" | \
  while read  FIELD_id  FIELD_link  FIELD_student_nb  FIELD_first_port; do
    # ---
    [[ -n $FIELD_id && -n $FIELD_link && -n $FIELD_student_nb && -n $FIELD_first_port ]] || continue
    # ---------
    # students
    # ---------
    # The last 4 characters of the link:
    PASSWORD_STUDENT=$(awk '{ match($0, /....$/, x); print x[0]; }' <<<"${FIELD_link}")
    OPTION_PASSWORD_STUDENT="-e VNC_PW=${PASSWORD_STUDENT}"
    # ---
    # The last 6 characters of the link (not the last 4 as for students):
    PASSWORD_TEACHER=$(awk '{ match($0, /......$/, x); print x[0]; }' <<<"${FIELD_link}")
    OPTION_PASSWORD_TEACHER="-e VNC_PW=${PASSWORD_TEACHER}"
    # ---
    for ((i=1; i<=$FIELD_student_nb ; i++)) ; do
      # ---
      CONTAINER_NAME=mariotel.id-${FIELD_id}.student-$i
      # ---
      # Status may be "created", "restarting", "running", "removing", "paused", "exited", or "dead":
      if [[ $EXPECTED_STATUS = "running" ]]; then
        ACTUAL_STATUS=$(docker_get_status $CONTAINER_NAME)
        if [[ $ACTUAL_STATUS = "running" || $ACTUAL_STATUS = "paused" ]]; then
          continue;  # do nothing for this container (jump to the next iteration)
        elif [[ $ACTUAL_STATUS = "exited" || $ACTUAL_STATUS = "dead" ]]; then
          docker restart $CONTAINER_NAME; continue;  # restart and jump to the next iteration
        fi # else treat this container with `docker run' (probably after a crash):
      fi
      # ---
      NO_VNC_EXPOSED_PORT=$((FIELD_first_port + i)) # Ex: i=1 => 26901
      # ---
      STUDENT_INDEX=$i
      OPTION_HOSTNAME="--hostname ws$(printf "%02d" $STUDENT_INDEX)"
      # ---
      {
      eval docker run -d \
        --name "${CONTAINER_NAME}" \
        "${OPTION_SHM}" \
        -p "${NO_VNC_EXPOSED_PORT}:${NO_VNC_LOCAL_PORT}" \
        "${OPTION_TMP}" \
        "${OPTION_CAP}" \
        "${OPTION_MEMORY_LIMITS}" \
        "${OPTION_CPU_LIMITS}" \
        "${OPTION_DISK_LIMITS}" \
        "${OPTION_USER_STUDENT}" \
        "${OPTION_PASSWORD_STUDENT}" \
        "${OPTION_HOSTNAME}" \
        "${IMAGE_VERSION}";
      # ---
      # Note: passwd+sudo for student should be an option:
      echo "student:${PASSWORD_STUDENT}" | docker exec --user 0:0 -i "${CONTAINER_NAME}" chpasswd;
      echo "teacher:${PASSWORD_TEACHER}" | docker exec --user 0:0 -i "${CONTAINER_NAME}" chpasswd;
      # ---
      provide_a_one_day_valid_certificate $CONTAINER_NAME \
        || echo "ERROR: something goes wrong providing an SSL certificate to $CONTAINER_NAME (error $?)" 1>&2;
      # ---
      } &
      # ---
    done # for
    # ---------
    # teacher
    # ---------
    for ((i=0; i<=0 ; i++)) ; do
      # ---
      CONTAINER_NAME=mariotel.id-${FIELD_id}.teacher-$i
      # ---
      # Status may be "created", "restarting", "running", "removing", "paused", "exited", or "dead":
      if [[ $EXPECTED_STATUS = "running" ]]; then
        ACTUAL_STATUS=$(docker_get_status $CONTAINER_NAME)
        if [[ $ACTUAL_STATUS = "running" || $ACTUAL_STATUS = "paused" ]]; then
          continue;  # do nothing for this container (jump to the next iteration)
        elif [[ $ACTUAL_STATUS = "exited" || $ACTUAL_STATUS = "dead" ]]; then
          docker restart $CONTAINER_NAME; continue;  # restart and jump to the next iteration
        fi # else treat this container with `docker run' (probably after a crash):
      fi
      # ---
      NO_VNC_EXPOSED_PORT=$((FIELD_first_port + i)) # Ex: i=1 => 26901
      # ---
      TEACHER_INDEX=$i
      OPTION_HOSTNAME="--hostname ws$(printf "%02d" $TEACHER_INDEX)"
      # ---
      {
      eval docker run -d \
        --name "${CONTAINER_NAME}" \
        "${OPTION_SHM}" \
        -p "${NO_VNC_EXPOSED_PORT}:${NO_VNC_LOCAL_PORT}" \
        "${OPTION_TMP}" \
        "${OPTION_CAP}" \
        "${OPTION_MEMORY_LIMITS}" \
        "${OPTION_CPU_LIMITS}" \
        "${OPTION_DISK_LIMITS}" \
        "${OPTION_USER_TEACHER}" \
        "${OPTION_PASSWORD_TEACHER}" \
        "${OPTION_HOSTNAME}" \
        "${IMAGE_VERSION}";
      # ---
      echo "teacher:${PASSWORD_TEACHER}" | docker exec --user 0:0 -i "${CONTAINER_NAME}" chpasswd;
      # ---
      provide_a_one_day_valid_certificate $CONTAINER_NAME \
        || echo "ERROR: something goes wrong providing an SSL certificate to $CONTAINER_NAME (error $?)" 1>&2;
      # ---
      } &
      # ---
    done # for
    # ---
    QUERY_UPDATE="update $TABLE_sessions set status = 'running' where id = ${FIELD_id};"
    mysql_cmd <<<"$QUERY_UPDATE"
    # ---
  done # while

} # starter()


# -------------------------------
#          stopper
# -------------------------------

function stopper {
local CURRENT_DATE QUERY_TO_BE_STARTED QUERY_UPDATE

CURRENT_DATE=$(date '+%Y-%m-%d %H:%M:%S')
QUERY_TO_BE_STOPPED="select id, link, student_nb from $TABLE_sessions where status = 'running' && '$CURRENT_DATE' >= DATE_ADD(finish, INTERVAL +10 MINUTE);"

mysql_cmd <<<"$QUERY_TO_BE_STOPPED" | \
  while read  FIELD_id  FIELD_link  FIELD_student_nb; do
    # ---
    [[ -n $FIELD_id && -n $FIELD_link && -n $FIELD_student_nb ]] || continue
    # ---------
    # students
    # ---------
    for ((i=1; i<=$FIELD_student_nb ; i++)) ; do
      # ---
      CONTAINER_NAME=mariotel.id-${FIELD_id}.student-$i
      # ---
      { docker rm -f "${CONTAINER_NAME}"; } &
      # ---
    done # for
    # ---------
    # teacher
    # ---------
    for ((i=0; i<=0 ; i++)) ; do
      # ---
      CONTAINER_NAME=mariotel.id-${FIELD_id}.teacher-$i
      # ---
      { docker rm -f "${CONTAINER_NAME}"; } &
      # ---
    done # for
    # ---
    QUERY_UPDATE="update $TABLE_sessions set status = 'finished' where id = ${FIELD_id};"
    mysql_cmd <<<"$QUERY_UPDATE"
    # ---
  done # while

} # stopper()


# -------------------------------
#        docker control
# -------------------------------

function get_websocket_connections {
  local i
  # ---
  for i in $(docker ps | awk 'NR>1 {print $NF}'); do
    SOME=$(docker exec --user 0:0 $i awk '/WebSocket connection$/ {print $1,$4,$5}' /dockerstartup/no_vnc_startup.log)
    if [[ -n $SOME ]]; then
      echo $i $SOME
    fi
  done
}

function kill_no_host_resolvable {
  local i
  # ---
  for i in $(docker ps | awk 'NR>1 {print $NF}'); do
    # ---
    docker exec --user 0:0 $i bash -c 'netstat -A inet -t -n -p  | awk -F "[ :/]*" "\$5 == \"6901\" && \$8==\"ESTABLISHED\" {print \$6, \$9}" | while read IP PID; do host $IP || { kill -9 $PID; iptables -t filter -A INPUT -p tcp -s $IP -j DROP; } done'
    # ---
  done | grep -i "not found"
}

# get_all_registered_ip_addresses_of_pattern IP_PATTERN [SESSION_ID]
function get_all_registered_ip_addresses_of_pattern {
  [[ $# -le 2 ]] || return 1
  local IP_PATTERN="$1"
  local SESSION_ID="$2"
  # --- Done as root:
  if [[ -z $SESSION_ID ]]; then
    mysql -s -B -D mariotel <<<"select distinct SUBSTRING_INDEX(remote_socket,':',1) from workstations where remote_socket like '$IP_PATTERN%:%';"
  else
    mysql -s -B -D mariotel <<<"select distinct SUBSTRING_INDEX(remote_socket,':',1) from workstations left join sessions on sessions.link = workstations.link where sessions.id = $SESSION_ID and remote_socket like '$IP_PATTERN%:%';"
  fi
}

# get_teachers_registered_ip_addresses_of_pattern IP_PATTERN
function get_teachers_registered_ip_addresses_of_pattern {
  [[ $# -le 1 ]] || return 1
  local IP_PATTERN="$1"
  # --- Done as root:
  mysql -s -B -D mariotel <<<"select distinct SUBSTRING_INDEX(remote_socket,':',1) from workstations where (workstation <= 0) and (remote_socket like '$IP_PATTERN%:%');"
}

# Could be also called manually by the administrator:
# --- Example:
# source session_manager.sh # this file
# protect_containers_from 94.239.215.28 mariotel.id-760.
# ---
function protect_containers_from {
  local DISTURBER_IP="$1"
  local PATTERN="$2"
  local i
  # ---
  for i in $(docker ps | awk -v PATTERN="$PATTERN" '(match($NF,PATTERN)!=0) {print $NF}'); do
    # ---
    # Add the iptables protected rule once:
    docker exec --user 0:0 $i bash -c "iptables -n -L INPUT | grep -q 'DROP .* $DISTURBER_IP ' || iptables -A INPUT -s $DISTURBER_IP/32 -j DROP"
    # ---
  done
}

# Something like "194.254.":
SERVER_IP_PREFIX_16_BITS=$(ip route list default | tr ' ' '\n' | grep -o '^[1-9][0-9.]*$' | awk -F. '{print $1"."$2"."}')
# ---
function is_IP_address_in_the_same_network {
  local IP="$1"
  local IP_PREFIX=$(awk -F. '{print $1"."$2"."}' <<<"$IP")
  [[ $IP_PREFIX = $SERVER_IP_PREFIX_16_BITS ]]
}

# ---
# Acceptable prefixes:  10.X.X.X  |----- 172.16.X.X - 172.31.X.X -----|  192.168.X.X
#   => patterns:        10.       172.1[6789].  172.2[0-9].  172.3[01].  192.168
function is_IP_address_private {
  local IP="$1"
  grep -q -E '^(10[.]|172[.]1[6789][.]|172[.]2[0-9][.]|172[.]3[01][.]|192[.]168)[0-9.][0-9.]*$' <<<"$IP"
}

# ---
function is_IP_address_whitelisted {
  is_IP_address_in_the_same_network "$1" || \
  is_IP_address_private "$1"
}

# -------------------------------
#             Main
# -------------------------------

# ---
SOURCEING && return
# else continue:

# -------------------------
#  Protect from intrusions
# -------------------------
# ---
# Observe the current connections and protect from intrusions if needed.
# A "disturber" is an IP address connected to the noVNC server of a container
# but not registered in the Mariotel's database and associated to a workstation.
# Teachers are allowed to be disturbers.
# ---
if [[ $1 = "-g" || $1 = "--get-connections" || $1 = "--get-websocket-connections" ]]; then
   # ---
   CONNECTIONS_LOGFILE="/var/log/mariotel.connections.log"
   set -x
   # --- Clean the log (prevent some strange lines with some strange characters to be saved):
   #grep '[a-zA-Z0-9./ \][-]*' $CONNECTIONS_LOGFILE 1>$CONNECTIONS_LOGFILE.clean
   #mv -f $CONNECTIONS_LOGFILE.clean $CONNECTIONS_LOGFILE
   # ---
   get_websocket_connections | grep "^mariotel.id-[^.]*[.]" 1> $CONNECTIONS_LOGFILE.current 2>&1
   # ---
   # Protect containers from casual disturbers:
   tr ' ' '\n' <$CONNECTIONS_LOGFILE.current | awk '$1 ~ /^[1-9][0-9.]*$/' | sort -n | \
     while read IP; do
       # ---
       CONTAINER_PATTERNS_TO_PROTECT=$(grep -w "$IP" $CONNECTIONS_LOGFILE.current | awk -F "[.]" '{print $1"."$2"."}' | sort -n | uniq)
       # ---
       # PATTERN will be something like "mariotel.id-754."
       for PATTERN in $CONTAINER_PATTERNS_TO_PROTECT; do
          # ---
          SESSION_ID=$(awk -F "[.-]" '{print $3; exit(0);}' <<<"$PATTERN") # 754
          # ---
          SAME_IP_EXPECTED="$(get_all_registered_ip_addresses_of_pattern $IP $SESSION_ID)"
          # ---
          if [[ $? = 0 && -z "$SAME_IP_EXPECTED" ]]; then
            # ---
            # We are in the case of the IP address has not been registered
            # in the database and associated to this session.
            # ---
            # Anyway, we trust addresses of the same network:
            WHITELISTED=$(is_IP_address_whitelisted "$IP" && echo YES)
            # ---
            # Also, we trust registered teachers, even if they are not involved, a priori, by this session:
            SAME_IP_EXPECTED="$(get_teachers_registered_ip_addresses_of_pattern $IP)"
            # ---
            if [[ $? = 0 && -z "$SAME_IP_EXPECTED" && -z "$WHITELISTED" ]]; then
              protect_containers_from "$IP" "$PATTERN"
            fi
            # ---
            # --- Report the accident
            TARGET="$CONNECTIONS_LOGFILE.disturbers"
            echo "$IP $(date +%Y-%m-%d) $PATTERN" 1>> $TARGET
            sort -g $TARGET | uniq > $TARGET.1; mv -f $TARGET.1 $TARGET
          fi
       done
     done
   # ---
   # Append to the global journal and remove duplicated:
   cat $CONNECTIONS_LOGFILE.current  1>> $CONNECTIONS_LOGFILE
   # ---
   sort -g $CONNECTIONS_LOGFILE | uniq > $CONNECTIONS_LOGFILE.1
   mv -f $CONNECTIONS_LOGFILE.1 $CONNECTIONS_LOGFILE
   exit 0
fi
# else continue:

# ---
if [[ $1 = "-k" || $1 = "--kill-no-host" || $1 = "--kill-no-host-resolvable" ]]; then
   NOHOST_LOGFILE="/var/log/mariotel.nohost.log"
   set +x
   # ---
   kill_no_host_resolvable 1>> $NOHOST_LOGFILE 2>&1
   # ---
   sort -g $NOHOST_LOGFILE | uniq > $NOHOST_LOGFILE.1
   mv -f $NOHOST_LOGFILE.1 $NOHOST_LOGFILE
   exit 0
fi
# else continue:

# -------------------------
#     Ordinary work
# -------------------------

# ---
# No more than one instance of this script may run at the same time:
# if [[ $(pgrep $(</proc/$$/comm) | wc -l) -gt 2 ]]; then exit 0; fi
# if ! am_I_the_only_running_instance; then exit 0; fi
# else continue:

echo "About to stop all finished sessions ..." 1>&2; # debugging
stopper

echo "About to restart all sessions expected to be running ..." 1>&2; # debugging
starter "running"

echo "About to start all sessions planned for now ..." 1>&2; # debugging
starter

# ---
wait
