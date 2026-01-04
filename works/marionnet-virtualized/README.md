# Marionnet-virtualized 🖥️🚀

Une machine virtuelle **Marionnet** basée sur **Debian 11** pour une expérience de virtualisation optimisée.

---

## Table des matières
- [Procédure de construction 🛠️](#procédure-de-construction-)
  - [1. Préparation de l'installation](#1-préparation-de-linstallation)
  - [2. Installation de l'environnement XFCE4](#2-installation-de-lenvironnement-xfce4)
  - [3. Configuration de l'autologin](#3-configuration-de-lautologin)
  - [4. Outils additionnels pour hyperviseurs](#4-outils-additionnels-pour-hyperviseurs)
  - [5. Outils pour VirtualBox](#5-outils-pour-virtualbox)
  - [6. Installation des dépendances de Marionnet](#6-installation-des-dépendances-de-marionnet)
  - [7. Installation de Marionnet](#7-installation-de-marionnet)
  - [8. Installation et configuration de XFCE Terminal](#8-installation-et-configuration-de-XFCE-Terminal)
  - [9. Installation de Firefox](#9-installation-de-firefox)
  - [10. Personnalisation du lanceur d'applications](#10-personnalisation-du-lanceur-dapplications)
  - [11. Optimisation système et démarrage](#11-optimisation-système-et-démarrage)
  - [12. Configuration finale et nettoyage](#12-configuration-finale-et-nettoyage)

---

## Procédure de construction 🛠️

### 1. Préparation de l'installation
- **Base** : ISO Debian 11 (amd64)
- **Compte utilisateur** : `etudiant/etudiant`
- **Installation minimale** : Installation sans environnement de bureau, avec serveur SSH et web

---

### 2. Installation de l'environnement XFCE4
Installez un bureau léger et efficace :
```bash
apt update
apt install xfce4 xfce4-goodies
```

---

### 3. Configuration de l'autologin
Activez l'autologin pour l'utilisateur `etudiant` :
```bash
nano /etc/lightdm/lightdm.conf
```
Ajoutez/modifiez :
```
autologin-user=etudiant
```

---

### 4. Installation des outils additionnels pour hyperviseurs
Installez les outils nécessaires pour divers hyperviseurs :
```bash
apt install open-vm-tools-desktop qemu-guest-agent hyperv-daemons spice-vdagent
```

---

### 5. Outils pour VirtualBox
Installez les outils spécifiques pour VirtualBox :
```bash
apt install build-essential dkms --no-install-recommends
bash VBoxLinuxAdditions.run
```

---

### 6. Installation des dépendances de Marionnet
Installez l'ensemble des dépendances nécessaires :
```bash
apt install flex bison gawk graphviz uml-utilities bzr opam liblablgtk3-ocaml-dev glade libgtksourceview-3.0-dev libtool bridge-utils gettext fonts-noto elementary-xfce-icon-theme rlfe vde2 libc6-i386 camlp4-extra --no-install-recommends
```

---

### 7. Installation de Marionnet
Téléchargez et installez Marionnet :
```bash
wget https://bazaar.launchpad.net/~marionnet-drivers/marionnet/trunk/download/head:/useful-scripts/marionnet_from_scratch
bash marionnet_from_scratch
sudo rm -R /root/.opam
```

---

### 8. Installation et configuration de XFCE Terminal
Remplacez `xterm` par `xfce4-terminal` pour une meilleure expérience :
```bash
apt remove xterm
```
Ensuite, activez le terminal XFCE dans Marionnet :
```bash
nano /etc/marionnet/marionnet.conf
```
Ajoutez la ligne suivante :
```
MARIONNET_TERMINAL="xfce4-terminal,-T,-e"
```

---

### 9. Installation de Firefox
Installez Firefox ESR sans dépendances supplémentaires :
```bash
apt install firefox-esr --no-install-recommends
```

---

### 10. Personnalisation du lanceur d'applications
- **Ajout du .desktop** sur le bureau
- **Personnalisation** via `alacarte` (catégorie *Éducation*)
```bash
apt install alacarte --no-install-recommends
```

---

### 11. Optimisation système et démarrage
- **Désactivation de la mise en veille et du verrouillage automatique**  
  _Via : Paramètres > Gestionnaire d'alimentation_

- **Activation de Marionnet au démarrage de XFCE**  
  Désactivez également le Notification Daemon et le verrouilleur d'écran  
  _Via : Paramètres > Session et démarrage_

- **Optimisation du démarrage avec GRUB**  
  Modifiez `/etc/default/grub` :
  ```bash
  nano /etc/default/grub
  ```
  Changez :
  ```
  GRUB_TIMEOUT=0
  ```
  Puis mettez à jour GRUB :
  ```bash
  update-grub
  ```

---

### 12. Configuration finale et nettoyage
- **Fond d’écran** : Choisissez le fond d’écran de l’Université Sorbonne Paris Nord (ex-Univ Paris 13) via l’interface graphique.
- **Permissions sudo** : Configurez l’utilisateur `etudiant` pour des droits sudo sans mot de passe.
  ```bash
  nano /etc/sudoers
  ```
  Ajoutez :
  ```
  etudiant ALL=(ALL) NOPASSWD: ALL
  ```
- **Redémarrage** : Vérifiez le bon fonctionnement.
  ```bash
  reboot
  ```
