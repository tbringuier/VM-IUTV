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

// Debugging:
ini_set('display_errors', 'on');

// Initialize the session
session_start();

// Check if the user is logged in, if not then redirect him to the login page
if(!isset($_SESSION["loggedin"]) || $_SESSION["loggedin"] !== true){
    header("location: login.php");
    exit;
}

// Check existence of the "id" parameter:
if(empty(trim($_GET["id"]))){
    // URL doesn't contain id parameter. Redirect to error page
    header("location: error.php?noway=t0");
    exit();
}

// Only the owner of the reservation can show details, except administrators:
if(($_SESSION["admin_level"] == 0) && $_SESSION["username"] !== $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["username"]) {
    header("location: error.php?noway=t1");
    exit();
}

// -----------------------
//   Confirm or details?
// -----------------------

// Check existence of the "confirm" parameter:
if((!empty($_GET["confirm"])) && ($_GET["confirm"] == 1)){
 $title="Confirmation";
 $page_title="Réservation confirmée";
}
else {
 $title="Détails";
 $page_title="Détails de la réservation";
}

// ---
$PREVENT_DATABASE_CONNECTION = True; // (use config.php as library)
require_once "config.php";
// ---
$cache = $_SESSION["mariotel_sessions_cache"][$_GET["id"]];  // TODO: Notice: Undefined offset: 404142 in /var/www/html/details.php on line 64 (bad id)
// ---
?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <title><?php echo $title;?></title>
    <!-- -->
    <script type="text/javascript" src="/node_modules/jquery/dist/jquery.min.js"></script>
    <script type="text/javascript" src="/node_modules/bootstrap/dist/js/bootstrap.min.js"></script>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <!-- -->
    <style type="text/css">
      body{ font: 14px sans-serif; } .wrapper{ width: 350px; padding: 20px; } td{ padding: 5px; }
      .glyphicon.glyphicon-info-sign { font-size: 20px; }
    </style>
    <script src="js/prepEntities.js"></script>
    <script>
      function mailto(email, cc, subject, body) {
          var url;
          url = 'mailto:' + email;
          url += '?cc=' + cc;
          url += '&subject=' + subject;
          url += '&body=' + body;
          window.open(url);
      }
      // --
      function mailto_text_body(email, cc, subject, body_element_id) {
          mailto(email, cc, subject, encodeURIComponent(document.getElementById(body_element_id).innerText));
      }
      // --
      function mailto_html_body(email, cc, subject, body_element_id) {
          mailto(email, cc, subject, escape(prepEntities(document.getElementById(body_element_id).innerHTML)));
          window.open(url);
      }
      // --
   </script>

</head>
<body>
    <!--  -->
    <div id="whole_content">
    <div class="container" id="common_section">
      <h2>Réservation de salles Mariotel<?php echo_info_anchor(); ?></h2>
      <h3><?php echo $page_title;?></h3>
      <h5><?php echo $cache["link"];?></h5>
      <br/>
      <!--  -->
      <div class="panel-group">
        <!--  -->
        <div class="panel panel-info">
          <div class="panel-heading">Réservation</div>
          <div class="panel-body">
            Responsable : <b><?php echo htmlspecialchars($cache["username"]); ?></b>,
            date : <?php echo htmlspecialchars($cache["starts"]); ?>,
            durée : <?php echo htmlspecialchars($cache["duration"]/60); ?> heures,
            nombre d'étudiants : <?php echo htmlspecialchars($cache["student_nb"]); ?>
          </div>
        </div>
      </div>
      <!--  -->
      <br/>
      <!--  -->
      <div class="panel-group">
        <!--  -->
        <div class="panel panel-success">
          <div class="panel-heading"><b>Lien d'accès étudiant</b></div>
          <div class="panel-body">
            <table>
                <tr><td>
                  <?php
                      $session_link = $cache["link"];
                      echo student_anchor_of_session_link($session_link);
                  ?>
                </td></tr>
                <tr><td>Mot de passe VNC/Unix : <b><?php echo htmlspecialchars(substr($cache["link"],-4)); ?></b> (4 derniers caractères du lien)</td></tr>
                <tr><td>Note : après la saisie de votre NOM-DE-FAMILLE, vous serez automatiquement connectés au système GNU/Linux</tr>
            </table>
          </div>
        </div>
      </div>
    </div> <!-- common_section -->
    <!-- -->
    <br/>
    <!-- -->
    <div class="container" id="teacher_section">
      <div class="panel-group">
        <!--  -->
        <div class="panel panel-warning">
          <div class="panel-heading"><b>Lien d'accès enseignant</b></div>
          <div class="panel-body">
            <table>
                <tr><td>
                  <?php
                      $session_link = $cache["link"];
                      echo teacher_anchor_of_session_link($session_link);
                  ?>
                </td></tr>
                <tr><td>Mot de passe VNC/Unix : <b><?php echo htmlspecialchars(substr($cache["link"],-6)); ?></b> (6 derniers caractères du lien)</td></tr>
                <tr><td>Note : vous serez l'utilisateur Unix <b>teacher</b> seulement sur votre poste, vous partagerez l'écran et l'identité Unix <b>student</b> sur tous les postes étudiants</tr>
            </table>
          </div>
        </div>
        <!--  -->
      </div>
    </div> <!-- teacher_section -->
    </div> <!-- whole_content -->
    <!--  -->
    <div class="container" id="buttons">
    <!-- -->
      <!-- <br/> -->
      <h4>Envoyer un email</h4>
      <!-- -->
      <div class="row">
        <div class="col">
          &nbsp;&nbsp;
          <button type="button" class="btn btn-secondary" title="Préparer un courriel à l'attention des étudiants (sans le lien enseignant)"
            onclick="mailto_text_body('', '<?php echo ($cache["email"]); ?>', '[Mariotel] Réservation <?php echo ($cache["link"]); ?>', 'common_section')"
            >mailto: students</button>
          &nbsp;&nbsp;
          <button type="button" class="btn btn-secondary" title="Préparer un courriel à l'attention de l'enseignant"
            onclick="mailto_text_body('<?php echo ($cache["email"]); ?>', '', '[Mariotel] Réservation <?php echo ($cache["link"]); ?>', 'whole_content')"
            >mailto: teacher</button>
          &nbsp;&nbsp;
        </div>
      </div>
      <!-- -->
      <!-- <br/> -->
      <hr>
      <?php make_toolbar_when(True); ?>
    </div>
    <!--  -->
</body>
</html>
