# ./hooks/build nux
# ./hooks/build nux --no-cache
# ./hooks/build dfw
# ./hooks/build dfw --no-cache
# ./hooks/build dev
# ./hooks/build dev --no-cache

### Build it locally like, for example:
### ./utils/util-hdx.sh Dockerfile 2
### Test it locally like, for example:
### ./hooks/test nux
### Result last line should be:
### + exit 0
### If 'exit 1' then adjust the version sticker variables in
### ./hooks/env

ARG BASETAG=18.04

FROM ubuntu:${BASETAG} as stage-ubuntu

# Man pages:
# Source: https://github.com/tianon/docker-brew-ubuntu-core/issues/122
RUN yes | unminimize && \
    apt-get install -y man-db && \
    rm -r /var/lib/apt/lists/*

ARG ARG_VCS_REF
ARG ARG_VERSION_STICKER

LABEL \
    maintainer="https://github.com/accetto" \
    vendor="accetto" \
    version-sticker="${ARG_VERSION_STICKER}" \
    org.label-schema.vcs-ref="${ARG_VCS_REF}" \
    org.label-schema.vcs-url="https://github.com/accetto/ubuntu-vnc-xfce"

### 'apt-get clean' runs automatically
RUN apt-get update && apt-get install -y \
        inetutils-ping \
        lsb-release \
        net-tools \
        unzip \
        vim \
        zip \
        curl \
        git \
        wget \
    && rm -rf /var/lib/apt/lists/*

### install current 'jq' explicitly
RUN \
    { \
    JQ_VERSION="1.6" ; \
    JQ_DISTRO="jq-linux64" ; \
    cd /tmp ; \
    wget -q "https://github.com/stedolan/jq/releases/download/jq-${JQ_VERSION}/${JQ_DISTRO}" ; \
    wget -q "https://raw.githubusercontent.com/stedolan/jq/master/sig/v${JQ_VERSION}/sha256sum.txt" ; \
    test=$(grep "${JQ_DISTRO}" sha256sum.txt | sha256sum -c | grep -c "${JQ_DISTRO}: OK") ; \
    if [[ $test -ne 1 ]] ; then \
        echo "FAILURE: ${JQ_DISTRO} failed checksum test" ; \
        exit 1 ; \
    else \
        rm sha256sum.txt ; \
        chown root "${JQ_DISTRO}" ; \
        chmod +x "${JQ_DISTRO}" ; \
        mv -f "${JQ_DISTRO}" /usr/bin/jq ; \
    fi ; \
    cd - ; \
    }

### next ENTRYPOINT command supports development and should be overriden or disabled
### it allows running detached containers created from intermediate images, for example:
### docker build --target stage-vnc -t dev/ubuntu-vnc-xfce:stage-vnc .
### docker run -d --name test-stage-vnc dev/ubuntu-vnc-xfce:stage-vnc
### docker exec -it test-stage-vnc bash
# ENTRYPOINT ["tail", "-f", "/dev/null"]

FROM stage-ubuntu as stage-xfce

ENV \
    DEBIAN_FRONTEND=noninteractive \
    LANG='fr_FR.UTF-8' \
    LANGUAGE='fr_FR:en' \
    LC_ALL='fr_FR.UTF-8'

### 'apt-get clean' runs automatically
RUN apt-get update && apt-get install -y \
        mousepad \
        locales \
        supervisor \
        xfce4 \
        xfce4-terminal \
        xfce4-genmon-plugin \
        thunar-archive-plugin \
    && locale-gen en_US.UTF-8 \
    && locale-gen fr_FR.UTF-8 \
    && locale-gen it_IT.UTF-8 \
    && locale-gen es_ES.UTF-8 \
    && apt-get purge -y pm-utils xscreensaver* \
    && rm -rf /var/lib/apt/lists/*

FROM stage-xfce as stage-vnc

### 'apt-get clean' runs automatically
### installed into '/usr/share/usr/local/share/vnc'
RUN wget -qO- https://dl.bintray.com/tigervnc/stable/tigervnc-1.10.1.x86_64.tar.gz | tar xz --strip 1 -C /

FROM stage-vnc as stage-novnc

### same parent path as VNC
ENV NO_VNC_HOME=/usr/share/usr/local/share/noVNCdim

### 'apt-get clean' runs automatically
### 'python-numpy' used for websockify/novnc
### ## Use the older version of websockify to prevent hanging connections on offline containers,
### see https://github.com/ConSol/docker-headless-vnc-container/issues/50
### installed into '/usr/share/usr/local/share/noVNCdim'
RUN apt-get update && apt-get install -y \
        python-numpy \
    && mkdir -p ${NO_VNC_HOME}/utils/websockify \
    && wget -qO- https://github.com/novnc/noVNC/archive/v1.1.0.tar.gz | tar xz --strip 1 -C ${NO_VNC_HOME} \
    && wget -qO- https://github.com/novnc/websockify/archive/v0.9.0.tar.gz | tar xz --strip 1 -C ${NO_VNC_HOME}/utils/websockify \
    && chmod +x -v ${NO_VNC_HOME}/utils/*.sh \
    && rm -rf /var/lib/apt/lists/*

### add 'index.html' for choosing noVNC client
RUN echo \
"<!DOCTYPE html>\n" \
"<html>\n" \
"    <head>\n" \
"        <title>noVNC</title>\n" \
"        <meta charset=\"utf-8\"/>\n" \
"    </head>\n" \
"    <body>\n" \
"        <p><a href=\"vnc_lite.html\">noVNC Lite Client</a></p>\n" \
"        <p><a href=\"vnc.html\">noVNC Full Client</a></p>\n" \
"    </body>\n" \
"</html>" \
> ${NO_VNC_HOME}/index.html

FROM stage-novnc as stage-wrapper

### 'apt-get clean' runs automatically
### Install nss-wrapper to be able to execute image as non-root user
RUN apt-get update && apt-get install -y \
        gettext \
        libnss-wrapper \
    && rm -rf /var/lib/apt/lists/*

# Marionnet dependencies and other nice tools (at the line starting by `nano') :
# terminator => too many installation problems
RUN apt-get update && apt-get install -y \
        gcc g++ make flex bison xterm aptitude graphviz uml-utilities libgtk2.0-dev libglade2-dev liblablgtksourceview2-ocaml-dev \
        libtool bridge-utils uml-utilities rlfe vde2 libc6-i386 overlay-scrollbar-gtk2 gtk2-engines-pixbuf gawk sudo rename \
        nano gedit gedit-plugins links lynx bc ipcalc \
        evince gimp inkscape meld libreoffice firefox \
        lyx texlive-pstricks dot2tex \
        emacs python-mode tuareg-mode ncurses-term fonts-noto fonts-noto-cjk-extra ghostscript-x default-jdk \
        ipython ipython3 ttf-bitstream-vera python-tk python3-tk python-scapy python3-scapy \
        python3-numpy gfortran python-numpy-doc python3-numpy-dbg python3-dev python3-nose \
        language-pack-fr language-pack-fr-base language-pack-gnome-fr language-pack-gnome-fr-base \
        language-pack-it language-pack-it-base language-pack-gnome-it language-pack-gnome-it-base \
        language-pack-es language-pack-es-base language-pack-gnome-es language-pack-gnome-es-base \
        xfce4-screenshooter gucharmap xdotool dconf-editor \
    && rm -rf /var/lib/apt/lists/* \
    && update-alternatives --install /usr/bin/python python /usr/bin/python2 1 \
    && update-alternatives --install /usr/bin/python python /usr/bin/python3 2

# ---
RUN apt-get update && apt-get install -y \
         chromium-browser chromium-browser-l10n chromium-codecs-ffmpeg \
         apt-transport-https ca-certificates curl gnupg-agent software-properties-common php libapache2-mod-php \
         php-mysql php-mbstring mariadb-server \
         postgresql postgresql-contrib

# Install ghostwriter on Ubuntu 18.04 (simple and nice markdown editor with preview):
RUN add-apt-repository ppa:wereturtle/ppa \
  && apt-get update && apt-get install -y \
     ghostwriter

# These packages create some troubles to ipython:
#          python3-pip \
#     && pip3 install --upgrade pip \
#     && pip3 install jupyter

# ---
# && echo 'root:Docker!'    | chpasswd
# && echo 'student:student' | chpasswd
# && echo 'teacher:teacher' | chpasswd
RUN  addgroup --gid 1001 student \
  && adduser  --disabled-password --gecos '' --uid 1001 --gid 1001 student \
  && addgroup --gid 1002 teacher \
  && adduser  --disabled-password --gecos '' --uid 1002 --gid 1002 teacher \
  && usermod -aG sudo teacher \
  && true
# ---

FROM stage-wrapper as stage-final

LABEL \
    any.accetto.description="Headless Ubuntu VNC/noVNC container with Xfce desktop" \
    any.accetto.display-name="Headless Ubuntu/Xfce VNC/noVNC container" \
    any.accetto.expose-services="6901:http,5901:xvnc" \
    any.accetto.tags="ubuntu, xfce, vnc, novnc"

### Arguments can be provided during build
ARG ARG_HOME
ARG ARG_REFRESHED_AT
ARG ARG_VERSION_STICKER
ARG ARG_VNC_BLACKLIST_THRESHOLD
ARG ARG_VNC_BLACKLIST_TIMEOUT
ARG ARG_VNC_PW
ARG ARG_VNC_RESOLUTION
ARG ARG_SSL_SUBJECT
ARG ARG_NO_VNC_SSLONLY

#  VNC_RESOLUTION=${ARG_VNC_RESOLUTION:-1360x768}
ENV \
    DISPLAY=:1 \
    HOME=${ARG_HOME:-/home/student} \
    NO_VNC_PORT="6901" \
    NO_VNC_SSLONLY=${ARG_NO_VNC_SSLONLY} \
    REFRESHED_AT=${ARG_REFRESHED_AT} \
    STARTUPDIR=/dockerstartup \
    VERSION_STICKER=${ARG_VERSION_STICKER} \
    VNC_BLACKLIST_THRESHOLD=${ARG_VNC_BLACKLIST_THRESHOLD:-20} \
    VNC_BLACKLIST_TIMEOUT=${ARG_VNC_BLACKLIST_TIMEOUT:-0} \
    VNC_COL_DEPTH=24 \
    VNC_PORT="5901" \
    VNC_PW=${ARG_VNC_PW:-headless} \
    VNC_RESOLUTION=${ARG_VNC_RESOLUTION:-1680x1050} \
    VNC_VIEW_ONLY=false \
    TERM=xterm

# marionnet_from_scratch
COPY [ "./src/startup/marionnet_from_scratch", "${STARTUPDIR}/" ]
RUN chmod +x "${STARTUPDIR}/marionnet_from_scratch" \
 && "${STARTUPDIR}"/marionnet_from_scratch -y

# ---
COPY [ "./src/startup", "${STARTUPDIR}/" ]
RUN chmod +x ${STARTUPDIR}/*.sh

# Adapt certificate's meta-informations to your organization setting ARG_SSL_SUBJECT.
# NOTE: This certificate should not actually be used. It should be replaced by another certificate created on the fly for each container.
RUN dd if=/dev/urandom of=$HOME/.rnd bs=256 count=1 \
  && openssl req -new -x509 -days $((365*2)) -nodes -subj "${ARG_SSL_SUBJECT}" -out ${NO_VNC_HOME}/self.pem -keyout ${NO_VNC_HOME}/self.pem \
  && chown root:root ${NO_VNC_HOME}/self.pem \
  && chmod a+r ${NO_VNC_HOME}/self.pem \
  && mv "${STARTUPDIR}/launch.sh" ${NO_VNC_HOME}/utils/launch.sh
# ---
ENV \
  ARG_SSL_SUBJECT="" \
  BASH_ENV=${STARTUPDIR}/bash_env.sh

### Create /home/student folder:
WORKDIR /home/student
### Preconfigure Xfce
COPY [ "./src/home/Desktop", "./Desktop/" ]
COPY [ "./src/home/config/xfce4/panel", "./.config/xfce4/panel/" ]
COPY [ "./src/home/config/xfce4/xfconf/xfce-perchannel-xml", "./.config/xfce4/xfconf/xfce-perchannel-xml/" ]
RUN gtk-update-icon-cache -f /usr/share/icons/hicolor
# ---

### Create /home/teacher folder:
WORKDIR /home/teacher
### Preconfigure Xfce
COPY [ "./src/home/Desktop", "./Desktop/" ]
COPY [ "./src/home/config/xfce4/panel", "./.config/xfce4/panel/" ]
COPY [ "./src/home/config/xfce4/xfconf/xfce-perchannel-xml", "./.config/xfce4/xfconf/xfce-perchannel-xml/" ]
RUN gtk-update-icon-cache -f /usr/share/icons/hicolor
# ---

### Fix permissions
RUN "${STARTUPDIR}"/set_user_permissions.sh "${STARTUPDIR}" /home/student /home/teacher

# ---building-time_last-settings.sh
RUN mkdir -p  /home/student /home/teacher \
 && chmod 755 /home/student /home/teacher \
 && chown -R student:student /home/student \
 && chown -R teacher:teacher /home/teacher \
 && chown root:root "${STARTUPDIR}/mariotel_services.sh" \
 && cat "$STARTUPDIR/bashrc_additions.sh" >> /home/student/.bashrc \
 && cat "$STARTUPDIR/bashrc_additions.sh" >> /home/teacher/.bashrc \
 && cat "$STARTUPDIR/bashrc_additions.sh" >> /root/.bashrc \
 && echo "%sudo  ALL=(ALL:ALL) NOPASSWD: /usr/sbin/service, /usr/sbin/a2enmod, /usr/bin/Xvnc, /bin/su, ${STARTUPDIR}/mariotel_services.sh" >> /etc/sudoers \
 && adduser student sudo \
 && adduser teacher sudo \
 && echo "default:x:1001:1001:Default Application User:/home/student:/bin/bash" >> /etc/passwd

# Fix last things:
RUN "${STARTUPDIR}"/building-time_last-settings.sh \
 && apt update && apt autoremove

# Mr proper (remove all "static" scripts, i.e. all scripts preparing the image):
RUN rm -f "${STARTUPDIR}/marionnet_from_scratch" \
 && rm -f "${STARTUPDIR}/set_user_permissions.sh" \
 && rm -f "${STARTUPDIR}/generate_container_user" \
 && rm -f "${STARTUPDIR}/building-time_last-settings.sh" \
 && apt update && apt upgrade -y \
 && dpkg -l | grep '^rc' | awk '{print $2}' | xargs dpkg --purge \
 && true

EXPOSE ${VNC_PORT} ${NO_VNC_PORT}

### Issue #7: Mitigating problems with foreground mode
WORKDIR ${STARTUPDIR}
ENTRYPOINT ["./vnc_startup.sh"]
CMD [ "--wait" ]
