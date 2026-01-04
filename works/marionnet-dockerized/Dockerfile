ARG BASEIMAGE=debian
ARG BASETAG=11

###############
### stage_cache
###############

FROM ${BASEIMAGE}:${BASETAG} AS stage_cache

### refresh the 'apt' cache
RUN rm -f /etc/apt/apt.conf.d/docker-clean ; \
    echo 'Binary::apt::APT::Keep-Downloaded-Packages "true" ;' > /etc/apt/apt.conf.d/keep-cache
RUN apt-get update

### embed the local '.g3-cache' from the build context
### note that the bound cache '/tmp/cache2' is ephemeral and all written data will be discarded automatically
### therefore copy its content into the another permanent cache '/tmp/g3-cache'
RUN \
    --mount=type=bind,target=/tmp/cache2 \
    mkdir -p /tmp/g3-cache \
    && if [ -d /tmp/cache2/.g3-cache/ ] ; then cp -r /tmp/cache2/.g3-cache/* /tmp/g3-cache/ ; fi


####################
### stage_essentials
####################

FROM ${BASEIMAGE}:${BASETAG} AS stage_essentials

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        gettext-base \
        gir1.2-rsvg-2.0 \
        jq \
        nano \
        procps \
        psmisc \
        sudo \
        tini \
        locales \
        wget

#################
### stage_xserver
#################

FROM stage_essentials AS stage_xserver

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        dbus-x11 \
        xauth \
        xinit \
        x11-xserver-utils \
        xdg-utils


##############
### stage_xfce
##############

FROM stage_xserver AS stage_xfce

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        xfce4 \
        xfce4-terminal


###############
### stage_tools
###############

FROM stage_xfce AS stage_tools

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        mousepad \
        python3 \
        systemctl \
        locales \
        ristretto \
        thunar-archive-plugin \
        xfce4-screenshooter \
        xfce4-terminal \
        xfce4-goodies \
        tumbler

#############
### stage_vnc
#############

FROM stage_tools AS stage_vnc

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/tmp/g3-cache/,target=/tmp/g3-cache/ \
    TIGERVNC_VERSION="1.13.1" \
    TIGERVNC_DISTRO="x86_64" \
    &&  if [ ! -s /tmp/g3-cache/tigervnc/tigervnc-"${TIGERVNC_VERSION}"."${TIGERVNC_DISTRO}".tar.gz ] ; then  \
            wget --show-progress --progress=bar:force:noscroll \
                -q https://sourceforge.net/projects/tigervnc/files/stable/"${TIGERVNC_VERSION}"/tigervnc-"${TIGERVNC_VERSION}"."${TIGERVNC_DISTRO}".tar.gz \
                -P /tmp/g3-cache/tigervnc ; \
        fi \
    &&  tar xzf /tmp/g3-cache/tigervnc/tigervnc-"${TIGERVNC_VERSION}"."${TIGERVNC_DISTRO}".tar.gz --strip 1 -C / \
    &&  ln -s /usr/libexec/vncserver /usr/bin/vncserver \
    &&  sed -i 's/exec(@cmd);/print "@cmd";\nexec(@cmd);/g' /usr/libexec/vncserver

ENV \
    DISPLAY="${ARG_VNC_DISPLAY:-:1}" \
    FEATURES_VNC=1 \
    VNC_COL_DEPTH="${ARG_VNC_COL_DEPTH:-24}" \
    VNC_PORT="${ARG_VNC_PORT:-5901}" \
    VNC_PW="" \
    VNC_RESOLUTION="${ARG_VNC_RESOLUTION:-1360x768}" \
    VNC_VIEW_ONLY="${ARG_VNC_VIEW_ONLY:-false}"

EXPOSE "${VNC_PORT}"


###############
### stage_novnc
###############

FROM stage_vnc AS stage_novnc
ARG ARG_APT_NO_RECOMMENDS
ARG ARG_NOVNC_PORT
ARG ARG_NOVNC_VERSION
ARG ARG_WEBSOCKIFY_VERSION

ENV \
    FEATURES_NOVNC=1 \
    NOVNC_HOME="/usr/libexec/noVNCdim" \
    NOVNC_PORT="6901"

RUN \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/cache/apt,target=/var/cache/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/var/lib/apt,target=/var/lib/apt \
    --mount=type=cache,from=stage_cache,sharing=locked,source=/tmp/g3-cache/,target=/tmp/g3-cache/ \
    DEBIAN_FRONTEND=noninteractive apt-get install --no-install-recommends -y \
        python3-numpy alacarte firefox-esr apache2 ipcalc gcc g++ make flex bison xterm gawk graphviz uml-utilities bzr opam liblablgtk3-ocaml-dev glade libgtksourceview-3.0-dev libtool bridge-utils net-tools gettext fonts-noto elementary-xfce-icon-theme rlfe libc6-i386 camlp4-extra vde2       \
    &&  mkdir -p "${NOVNC_HOME}"/utils/websockify \
    &&  NOVNC_VERSION="1.5.0" \
    &&  WEBSOCKIFY_VERSION="0.13.0" \
    &&  if [ ! -s /tmp/g3-cache/novnc/v"${NOVNC_VERSION}".tar.gz ] ; then \
            wget --show-progress --progress=bar:force:noscroll \
                -q https://github.com/novnc/noVNC/archive/v"${NOVNC_VERSION}".tar.gz \
                -P /tmp/g3-cache/novnc ; \
        fi \
    &&  if [ ! -s /tmp/g3-cache/websockify/v"${WEBSOCKIFY_VERSION}".tar.gz ] ; then \
            wget --show-progress --progress=bar:force:noscroll \
                -q https://github.com/novnc/websockify/archive/v"${WEBSOCKIFY_VERSION}".tar.gz \
                -P /tmp/g3-cache/websockify ; \
        fi \
    &&  tar xzf /tmp/g3-cache/novnc/v"${NOVNC_VERSION}".tar.gz --strip 1 -C "${NOVNC_HOME}" \
    &&  tar xzf /tmp/g3-cache/websockify/v"${WEBSOCKIFY_VERSION}".tar.gz --strip 1 -C "${NOVNC_HOME}"/utils/websockify \
    &&  chmod 755 -v "${NOVNC_HOME}"/utils/novnc_proxy

### add 'index.html' for choosing noVNC client
RUN echo \
"<!DOCTYPE html>\n\
<html>\n\
    <head>\n\
        <title>noVNC</title>\n\
        <meta charset=\"utf-8\"/>\n\
        <meta http-equiv=\"refresh\" content=\"1; url='vnc.html?password='\" />\n\
    </head>\n\
    <body>\n\
    <p>Loading...</p>\n\
    </body>\n\
</html>" \
> "${NOVNC_HOME}"/index.html

EXPOSE "${NOVNC_PORT}"


###################
### merge_stage_vnc
###################

FROM stage_novnc AS merge_stage_vnc

ENV \
    HEADLESS_USER_ID="1001" \
    HEADLESS_USER_NAME="etudiant" \
    HEADLESS_USER_GROUP_ID="1001" \
    HEADLESS_USER_GROUP_NAME="etudiant" \
    SUDO_INITIAL_PW=etudiant \
    HOME="/home/etudiant"

WORKDIR "${HOME}"

### add '.bashrc' and similar resources
COPY ./src/home "${HOME}"/


###############
### FINAL STAGE
###############

FROM merge_stage_vnc AS stage_final

ENV \
    FEATURES_OVERRIDING_ENVV=0 \
    FEATURES_VERSION_STICKER=1 \
    STARTUPDIR="/dockerstartup"

COPY ./src/xfce-startup "${STARTUPDIR}"/
COPY ./src/tests "${HOME}"/tests/
COPY ./xfce/src/home/Desktop "${HOME}"/Desktop/

COPY ./xfce/src/home/config "${HOME}"/.config/

### Note that the line 'chmod 666 /etc/passwd /etc/group' sets the "softer" permissions only temporary.
### It allows the user generator startup script to configure the user and the group correctly.
### The script will set the permissions of both files back to the default '644'.
### The script will also clear the file '.initial_sudo_password' after using it.
### However, note that the initial sudo password will still be persisted in the image history.
### You have to change it inside the container, if you want to keep it really secret.
### Note that all this will not be done, if the startup script will not be executed.
RUN chmod 666 /etc/passwd /etc/group

RUN echo "${HEADLESS_USER_GROUP_NAME}:x:${HEADLESS_USER_GROUP_ID}:" >> /etc/group

RUN echo "${HEADLESS_USER_NAME}:x:${HEADLESS_USER_ID}:${HEADLESS_USER_GROUP_ID}:Étudiant:${HOME}:/bin/bash" >> /etc/passwd

RUN echo "${HEADLESS_USER_NAME}  ALL=(ALL:ALL) ALL" | sudo tee /etc/sudoers.d/"${HEADLESS_USER_NAME}"

RUN echo "${SUDO_INITIAL_PW:-headless}" > "${STARTUPDIR}"/.initial_sudo_password

RUN echo "${HEADLESS_USER_NAME}:$(cat ${STARTUPDIR}/.initial_sudo_password)" | chpasswd

RUN "${STARTUPDIR}"/set_user_permissions.sh "${STARTUPDIR}" "${HOME}"

USER "${HEADLESS_USER_ID}"

###############
### MARIONNET !!!
###############

FROM stage_final AS ready_to_run

USER "0"

RUN echo "etudiant ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN sed -i -e 's/# fr_FR.UTF-8 UTF-8/fr_FR.UTF-8 UTF-8/' /etc/locale.gen && \
    dpkg-reconfigure --frontend=noninteractive locales && \
    update-locale LANG=fr_FR.UTF-8

USER "${HEADLESS_USER_ID}"

ENV LANG fr_FR.UTF-8

VOLUME /dev/shm
VOLUME /tmp

ENTRYPOINT [ "/usr/bin/tini", "--", "/dockerstartup/startup.sh" ]