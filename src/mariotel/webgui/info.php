<?php
// ---
// This file is part of Mariotel
// Copyright (C) 2020  Jean-Vincent Loddo
// Copyright (C) 2020  Université Sorbonne Paris Nord
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// ---
?>
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title>Mariotel</title>
    <script type="text/javascript" src="/node_modules/jquery/dist/jquery.min.js"></script>
    <script type="text/javascript" src="/node_modules/bootstrap/dist/js/bootstrap.min.js"></script>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
      .image_container {
          position: relative;
          text-align: center;
          }
      .resize_fit_center_40pct { max-width:40%; max-height:40%; vertical-align: middle; }
      .resize_fit_center_18pct { max-width:18%; max-height:18%; vertical-align: middle; }
      .resize_fit_center_5pct { max-width:5%; max-height:5%; }
    </style>
</head>
<body>

<div class="container">

  <h2>Projet Mariotel</h2>
  <br/>
  <p>Mariotel est un logiciel libre (GNU GPL) de <b>gestion de salles de TP virtuelles</b> conçu et développé par Jean-Vincent Loddo (maître de conférences
  au <a href="https://lipn.univ-paris13.fr/" target="_blank" title="Laboratoire d'Informatique de Paris Nord">LIPN</a> de
  l'<a href="https://www.univ-paris13.fr/" target="_blank">Université Sorbonne Paris Nord</a>).
  Avec Mariotel, il est possible pour un enseignant de réserver une salle d'ordinateurs virtuels afin de réaliser des travaux pratiques sous <b>GNU/Linux</b>.
  Les ordinateurs seront ensuite rendus disponibles à la date réservée et <b>accessibles à distance</b> par les étudiants et l'enseignant avec
  un <b>simple navigateur Internet</b> (<em>firefox</em>, <em>chrome</em>, <em>safari</em>, etc).
  <div class="image_container">
    <a href="images/screenshot_session_active.png" target="_blank"><img src="images/screenshot_session_active.png" alt="Paris" class='resize_fit_center_40pct'></a>
  </div>
  <br/>
  <p>La distribution actuellement utilisée sur les ordinateurs virtuels est la <b>XUbuntu 18.04</b> où sont installés plusieurs logiciels
     utilisables pour des travaux pratiques sous GNU/Linux (<em>firefox</em>, <em>libreoffice</em>, <em>python</em>, <em>gcc</em>, <em>java</em>, <em>emacs</em>, <em>marionnet</em>, ...). L'enseignant qui réserve la session peut choisir le nombre de postes (ou "stations") nécessaires à sa séances de TP. Lorsque la séance est en cours d'exécution, chaque étudiant se connecte en précisant son nom et l'enseignant peut ainsi, si besoin, se connecter en simultané au même poste pour <b>aider l'étudiant</b> dans la réalisation du TP.
  </p>
  <!-- -->
  <div class="panel-group">
    <div class="panel panel-info">
    <!-- -->
    <div class="panel panel-success">
      <div class="panel-heading">Captures d'écran</div>
      <div class="panel-body">
        <div class="image_container">
          <!-- -->
          <a href="images/screenshot_acces_etudiant_step_1.png" target="_blank" title="Accès étudiant, saisie du nom"><img src="images/screenshot_acces_etudiant_step_1.png" alt="Mariotel" class='resize_fit_center_18pct'></a>&nbsp;
          <!-- -->
          <a href="images/screenshot_session_active.png" target="_blank" title="Session active, vue de l'enseignant"><img src="images/screenshot_session_active.png" alt="Mariotel" class='resize_fit_center_18pct'></a>&nbsp;
          <!-- -->
          <a href="images/screenshot_acces_etudiant_step_2.png" target="_blank" title="Accès étudiant, connexion à la station de travail"><img src="images/screenshot_acces_etudiant_step_2.png" alt="Mariotel" class='resize_fit_center_18pct'></a>&nbsp;
          <!-- -->
          <a href="images/screenshot_session_etudiant_marionnet.png" target="_blank" title="Session étudiant, on travaille avec Marionnet"><img src="images/screenshot_session_etudiant_marionnet.png" alt="Mariotel" class='resize_fit_center_18pct'></a>&nbsp;
          <!-- -->
          <a href="images/screenshot_session_etudiant_python.png" target="_blank" title="Session étudiant, on travaille avec Python"><img src="images/screenshot_session_etudiant_python.png" alt="Mariotel" class='resize_fit_center_18pct'></a>&nbsp;
          <!-- -->
        </div>
      </div>
    </div>
    <!-- -->
    <div class="panel-heading">Remerciements</div>
      <div class="panel-body">
        Merci à Xavier Monnin (CNRS, responsable des réseaux LIPN et LAGA à l'Institut Galilée, USPN) pour la construction du système
        hébergeant la première version de Mariotel en juillet 2020.
        Merci à Rushed Kanawati (chef du département Réseaux et Télécom à l'IUT de Villetaneuse, USPN) pour l'idée
        &laquo; d'utiliser <a href="https://www.marionnet.org/" target="_blank">Marionnet</a> dans un navigateur web &raquo;.
        <br/>
        <br/>
        <!-- -->
        <a href="https://lipn.univ-paris13.fr/" target="_blank" title="Laboratoire d'Informatique de Paris Nord"><img src="images/logo-LIPN-plein.png" alt="LIPN" class='resize_fit_center_5pct'></a>
        Merci au Laboratoire d'Informatique de Paris Nord (<a href="https://lipn.univ-paris13.fr/" target="_blank" title="Laboratoire d'Informatique de Paris Nord">LIPN</a>) pour l'ensemble du support fourni.
        <!-- -->
        <br/>
        <br/>
        Merci aux projets de la <a href="http://www.gnu.org/philosophy/free-sw.html" target="_blank">free software</a> et, en général, à tout le mouvement du logiciel libre pour plusieurs éléments de grande qualité qui sont utilisés dans Mariotel (qui n'est qu'une fine surcouche logicielle au dessus de ces géants). En particulier :
        <ul style="list-style-type:disc;">
          <!-- -->
          <li>le projet <a href="https://github.com/novnc/noVNC" target="_blank">noVNC</a> permettant le déport de l'affichage graphique
              d'un système GNU/Linux sur un navigateur
          </li>
          <!-- -->
          <li>le projet <a href="https://www.docker.com/" target="_blank">Docker</a> permettant l'isolation d'un système GNU/Linux dans un conteneur</li>
          <!-- -->
          <li>le projet <a href="https://github.com/accetto/ubuntu-vnc-xfce/" target="_blank">Accetto</a> permettant d'automatiser la construction de conteneurs
          docker <a href="https://en.wikipedia.org/wiki/Headless_computer" target="_blank">headless</a> de type Ubuntu/Xfce avec noVNC</li>
          <!-- -->
          <li>le projet <a href="http://www.apache.org/" target="_blank">Apache</a> fournissant un serveur HTTP extrêmement efficace et paramétrable, capable d'interpréter plusieurs langages, dont PHP</li>
          <!-- -->
          <li>le projet <a href="https://mariadb.org/" target="_blank">MariaDB</a> fournissant le système de gestion de base de données (sous licence GPL)</li>
          <!-- -->
          <li>le projet <a href="https://www.php.net/" target="_blank">PHP</a> fournissant un langage de programmation pour applications web</li>
          <!-- -->
          <li>le projet <a href="https://getbootstrap.com/" target="_blank">Bootstrap</a> fournissant des briques logicielles HTML, CSS et JavaScript pour le développement d'applications web</li>
          <!-- -->
          <li>le projet <a href="https://git-scm.com/" target="_blank">Git</a> pour la gestion décentralisée des versions</li>
          <!-- -->
          <li>les distributions <a href="https://www.debian.org/" target="_blank">Debian</a> (pour le système serveur) et
                                <a href="https://ubuntu.com/"     target="_blank">Ubuntu</a> (pour les conteneurs Docker) </li>
          <!-- -->
          <li>les projets <a href="https://www.gnu.org/" target="_blank">GNU</a> et <a href="https://www.kernel.org/" target="_blank">Linux</a>
               à l'origine des précédents</li>
        </ul>
      </div>
    </div>
    <!-- -->
  </div>
  <a class="btn btn-link" href="javascript:history.go(-1)">Retour</a>
  <br/>
</div>
</body>
</html>
