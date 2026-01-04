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

$PREVENT_DATABASE_CONNECTION = True; // (use config.php as library)
require_once "config.php";

?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Erreur</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css"> body{ font: 14px sans-serif; } .wrapper{ width: 350px; padding: 20px; } </style>
</head>
<body>
    <!--  -->
    <div class="container">
      <h2>Réservation de salles Mariotel</h2>
      <br/>
      <br/>
      <h2><b>Erreur</b></h2>
      <p><b>Une erreur est survenue pendant la procédure de réservation</b></p>
      <div class="panel-group">
        <!--  -->
        <div class="panel panel-warning">
          <div class="panel-heading">Votre réservation</div>
          <div class="panel-body">
            Responsable: <b><?php echo htmlspecialchars($_SESSION["username"]); ?></b>,
            date: <?php echo htmlspecialchars($_SESSION["booking"]["date_time"]);  ?>,
            durée: <?php echo htmlspecialchars($_SESSION["booking"]["duration"]);   ?> heures,
            nombre d'étudiants: <?php echo htmlspecialchars($_SESSION["booking"]["student_nb"]); ?>
          </div>
        </div>
        <!--  -->
        <div class="panel panel-danger">
          <div class="panel-heading">Lien concerné</div>
          <div class="panel-body">
            <?php echo htmlspecialchars($_SESSION["booking"]["link"]); ?>
          </div>
        </div>
        <!--  -->
        <div class="panel panel-info">
          <div class="panel-heading">Explications possibles</div>
          <div class="panel-body">Il est possible que cela soit dû à une réservation en double, du même utilisateur au même horaire</div>
        </div>
        <!--  -->
      </div>
    </div>
    <!--  -->
    <br><br>
    <?php make_toolbar_when(True); ?>
    <br><br>
    <!--  -->
</body>
</html>
