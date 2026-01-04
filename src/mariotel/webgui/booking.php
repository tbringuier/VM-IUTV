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

date_default_timezone_set('Europe/Paris');

// Check if the user has already reserved providing all required informations.
// In this case redirect him to the confirmation page.

// Define variables and initialize with empty values
$date_time = $duration = $student_nb = "";
$date_time_err = $duration_err = $student_nb_err = "";

// Processing form data when form is submitted
if($_SERVER["REQUEST_METHOD"] == "POST"){

    // var_dump($_POST);

    // Check if date_time is empty
    if(empty(trim($_POST["date_time"]))){ $date_time_err = "Please enter date and time."; } else{ $date_time = trim($_POST["date_time"]); }

    // Check if duration is empty
    if(empty(trim($_POST["duration"]))){ $duration_err = "Please enter the duration."; } else{ $duration = trim($_POST["duration"]); }

    // Check if student_nb is empty
    if(empty(trim($_POST["student_nb"]))){ $student_nb_err = "Please enter the student number."; } else{ $student_nb = trim($_POST["student_nb"]); }

    // Validate credentials
    if(empty($date_time_err) && empty($duration_err) && empty($student_nb_err)){
      // ---
      // Store data in session variables:
      $_SESSION["booking"] = $_POST;
      // ---
      header("location: confirm.php");
      exit;
    }
}

// Provide (or not) the button to add a new user:
$register_button="";
if ((isset($_SESSION["admin_level"])) && ($_SESSION["admin_level"] >= 1)){
  $register_button='<a href="register.php" class="btn btn-default" title="Ajouter un nouvel utilisateur (enseignant)">Inscrire</a>&nbsp;&nbsp;';
}

// Only admin with level>=1 may exceed some limits:
$duration_max=5;
if ((isset($_SESSION["admin_level"])) && ($_SESSION["admin_level"] >= 1)){
  $duration_max=10; // 2 times the limit
}

// Only admin with level>=2 may exceed some limits:
$student_nb_max=32;
if ((isset($_SESSION["admin_level"])) && ($_SESSION["admin_level"] >= 2)){
  $student_nb_max=320; // 10 times the limit
}

// ---
$PREVENT_DATABASE_CONNECTION = True; // (use config.php as library)
require_once "config.php";
// ---

?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Réservation</title>
    <!-- bootstrap-datetimepicker -->
    <script type="text/javascript" src="/node_modules/jquery/dist/jquery.min.js"></script>
    <script type="text/javascript" src="/node_modules/moment/min/moment-with-locales.min.js"></script>
    <script type="text/javascript" src="/node_modules/bootstrap/dist/js/bootstrap.min.js"></script>
    <script type="text/javascript" src="/node_modules/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min.js"></script>
    <script type="text/javascript" src="/node_modules/eonasdan-bootstrap-datetimepicker/src/js/bootstrap-datetimepicker.js"></script>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <link rel="stylesheet" href="/node_modules/eonasdan-bootstrap-datetimepicker/build/css/bootstrap-datetimepicker.css" />
    <!-- -->
    <style type="text/css">
      .container{ width: 600px; margin: 0 auto; }
      body{ font: 14px sans-serif; text-align: center; }
     .glyphicon.glyphicon-info-sign { font-size: 20px; }
    </style>
    <!-- -->
    <script>
       var one_year_later = new Date();
       one_year_later.setFullYear(one_year_later.getFullYear() + 1);
      // ---
       var five_minutes_later = new Date();
       var five_minutes_later = new Date(five_minutes_later.getTime() + 5 * 60000);
      // ---
      // See: http://eonasdan.github.io/bootstrap-datetimepicker/Options/
      // ---
      $(function () {
        $('#datetimepicker1').datetimepicker({
              format: "YYYY-MM-DD HH:mm",
              minDate:five_minutes_later,
              maxDate:one_year_later,
              collapse:false, // sideBySide:true,
              calendarWeeks:true,
              locale:"fr"
           });
    });
    </script>
    <!-- -->
</head>
<body>
    <div class="page-header">
        <h2>Bonjour <b><?php echo htmlspecialchars($_SESSION["username"]); ?></b>, bienvenue sur la page de</h2>
        <h2>réservation d'une salle Mariotel<?php echo_info_anchor(); ?></h2>
        <br/>
    </div>
    <!-- -->
    <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post">
        <!-- -->
        <div class="container">
          <div class="panel panel-primary">
            <div class="panel-heading">Votre réservation</div>
              <div class="panel-body">
                <div class="row">
                    <!-- -->
                    <div class='col-md-6'>
                      <div class="form-group">
                          <label class="control-label">Date et heure</label>
                          <div class='input-group date' id='datetimepicker1'>
                            <input type='text' class="form-control" id="date_time" name="date_time" />
                            <span class="input-group-addon">
                            <span class="glyphicon glyphicon-calendar"></span>
                            </span>
                          </div>
                      </div>
                    </div>
                    <!-- -->
                    <div class="col-md-6">
                      <div class="form-group">
                          <label class="control-label">Durée</label>
                          <input type="number" class="form-control" name="duration" id="duration" min="0.5" max="<?php echo ($duration_max); ?>" value="3" step="0.5" >
                      </div>
                    </div>
                    <!-- -->
                </div>
                <!-- -->
                <div class="row">
                    <div class="col-md-6">
                      <div class="form-group">
                          <label class="control-label">Nombre d'étudiants</label>
                          <input type="number" class="form-control" name="student_nb" id="student_nb" min="0" max="<?php echo ($student_nb_max); ?>" value="25">
                      </div>
                    </div>
                    <!-- -->
                    <div class="col-md-6">
                      <div class="form-group">
                          <label class="control-label">Email de notification du lien</label>
                          <input type="text" class="form-control" name="email" id="email" value="<?php echo ($_SESSION["email"]); ?>" >
                      </div>
                    </div>
                    <!-- -->
                </div>
                <!-- -->
                <input type="submit" class="btn btn-success" value="Réserver">
                <!-- -->
              </div>
          </div>
        </div>
    </form>
    <!-- -->
    <br/><br/>
    <?php make_toolbar_when(True, False, True); ?>
</body>
</html>
