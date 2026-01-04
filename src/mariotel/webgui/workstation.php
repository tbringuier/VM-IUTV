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

// Initialize the student session:
session_start();

// --- Recover data stored in session.php:
// ---
if (!($row = $_SESSION["row"])) {
  header("location: error.php?noway=w0");
  exit();
}
// ---
$session_link = $row['link'];
$student_nb   = $row['student_nb'];
$first_port   = $row['first_port'];
// ---
$starts_week_nb = $_SESSION["GLOBALS"]["starts_week_nb"];
$french_session_status = $_SESSION["GLOBALS"]["french_session_status"];
$starts_day_name_locale = $_SESSION["GLOBALS"]["starts_day_name_locale"];
$starts_day = $_SESSION["GLOBALS"]["starts_day"];
$starts_time = $_SESSION["GLOBALS"]["starts_time"];
$finish_time = $_SESSION["GLOBALS"]["finish_time"];
// ---

// Debugging:
ini_set('display_errors', 'on');

date_default_timezone_set('Europe/Paris');

// Check if the user has already reserved providing all required informations.
// In this case redirect him to the confirmation page.

function virtual_machine_location($first_port, $workstation_index) {
  // ---
  $port = $first_port + $workstation_index;
  $result = 'http://'.$_SERVER['SERVER_NAME'].':'.$port.'/'.'vnc.html';
  // ---
  return $result;
}

function is_student_name_acceptable($candidate) {
  // ---
  $candidate = trim($candidate);
  $result = (! empty($candidate)) && (preg_match('/[a-zA-Z][a-zA-Z -]+/', $candidate));
  return $result;
}

// Processing form data when form is submitted:
if (($_SERVER["REQUEST_METHOD"] == "POST") && (is_student_name_acceptable($_POST["student_name"]))) {

    // Include config file:
    require_once "config.php";

    // Only students with reversible IP address may access.
    // We accept also IP localized in the same country or private IP (same organization):
    if (! test_reverse_ipv4_lookup_or_same_country_or_private($_SERVER["REMOTE_ADDR"])) {
        // ---
        header("location: error.php?noway=w1");
        exit();
    }

    $student_name = formatted_student_name($_POST["student_name"]);
    $first_letter  = substr(ucfirst($student_name), 0, 1);
    $remote_socket = $_SERVER["REMOTE_ADDR"] . ":" . $_SERVER["REMOTE_PORT"];

    // Store data in session variables:
    $_SESSION["student_name"] = $student_name;

    // mysql> DESCRIBE workstations;
    // +---------------+--------------+------+-----+---------------------+----------------+
    // | Field         | Type         | Null | Key | Default             | Extra          |
    // +---------------+--------------+------+-----+---------------------+----------------+
    // | id            | int(11)      | NO   | PRI | NULL                | auto_increment |
    // | link          | varchar(255) | NO   | MUL | NULL                |                |
    // | workstation   | int(11)      | NO   |     | NULL                |                |
    // | allowed_user  | varchar(50)  | NO   |     | NULL                |                |
    // | remote_socket | varchar(50)  | NO   |     | NULL                |                |
    // | created_at    | datetime     | YES  |     | current_timestamp() |                |
    // +---------------+--------------+------+-----+---------------------+----------------+
    // 6 rows in set (0.001 sec)

    // Try to get an already registered place for the student:
    $sql = "SELECT workstation FROM workstations WHERE " . "link = '" . $session_link . "'" . " AND " . "allowed_user = '" . $student_name . "'";
    // --
    if ((($result = mysqli_query($link, $sql)) && (mysqli_num_rows($result) === 1) && ($ws = mysqli_fetch_array($result)))) {
      $FOUND = True;
      $WORKSTATION_INDEX = $ws["workstation"]; // 1..($student_nb)
    }
    else {
      // Search for a free place:
      $sql = "INSERT INTO workstations (link, workstation, allowed_user, remote_socket) VALUES (?, ?, ?, ?)";
      // ---
      $FOUND = False;
      $ATTEMPTS=0;
      $START = round((ord($first_letter) - ord("A")) / (ord("Z") - ord("A")) * $student_nb);
      $START = max(1, min($student_nb, $START));
      $INDEX = $START - 1;               // 0..($student_nb-1)
      $WORKSTATION_INDEX = ($INDEX + 1); // 1..($student_nb)
      // Search a place forward or backward according to the value of starting candidate:
      $DIRECTION = (($START > ($student_nb/2)) ? (-1) : 1);
      // ---
      while ((! $FOUND) && ($ATTEMPTS <= $student_nb)) {

        if(!($stmt = mysqli_prepare($link, $sql))) {
          // Strange problem:
          header("location: error.php?noway=w2");
          exit();
        }

        // Bind variables to the prepared statement as parameters:
        mysqli_stmt_bind_param($stmt, "siss", $session_link, $WORKSTATION_INDEX, $student_name, $remote_socket);
        $ATTEMPTS++;

        // Attempt to execute the prepared statement:
        if(mysqli_stmt_execute($stmt)){ $FOUND = True; } else {
          $INDEX = ($INDEX + $DIRECTION + $student_nb) % $student_nb; // 0..($student_nb-1)
          $WORKSTATION_INDEX = ($INDEX + 1);                          // 1..($student_nb)
          }

        // Close statement
        mysqli_stmt_close($stmt);

      } // while

      // ---
      if (! $FOUND) {
        // TODO: do something more explicit! (something like: "Sorry, all workstation are booked, there are no available free places in the room")
        header("location: error.php?noway=w3"); exit();
        }
    }

    // ---
    // Go to the assigned workstation:
    header("location: " . virtual_machine_location($first_port, $WORKSTATION_INDEX));
    exit;
    }

// ---
// Note: if this point is reached, the student has still to provide its last name:
// ---
?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Session Mariotel</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .container{ width: 500px; margin: 0 auto; }
        .page-header h2{ margin-top: 0; }
        table tr td:last-child a{ margin-right: 11px; }
        th{ text-align: center; }
        body{ font: 14px sans-serif; text-align: center; }
    </style>

</head>
<body>
    <div class="page-header">
        <h2>Accès à la salle virtuelle Mariotel</h2>
        <h6><?php echo $session_link;?></h6>
        <h5>Réservée par <b><?php echo $row['username']; ?></b>, <?php echo $row['student_nb'];?> postes,
            <b><?php echo $french_session_status[$row['status']];?></b></h5>
        <h5>Pour la semaine <?php echo $starts_week_nb.", <b>".$starts_day_name_locale." ".$starts_day."</b>";?>
        de <?php echo $starts_time;?> à <?php echo $finish_time;?></h5>
    </div>
    <!-- -->
    <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post">
        <!-- -->
        <div class="container">
          <div class="panel panel-primary">
              <div class="panel-heading">Entrez votre nom pour accéder à un poste de travail</div>
              <div class="panel-body">
                <!-- -->
                <div class="row align-items-center">
                    <!-- -->
                    <div class="col-md-3">
                       <img src="images/ico.machine.on.med.png">
                    </div>
                    <!-- -->
                    <div class="col-md-6">
                      <span class="input-group-addon"><i class="glyphicon glyphicon-user"></i></span>
                      <input id="student_name" type="text" class="form-control" name="student_name" placeholder="Nom de famille"
                             pattern="[a-zA-Z]{3}[a-zA-Z .-]*" title="Au moins trois lettres svp" required>
                    </div>
                    <!-- -->
                    <div class="col-md-3">
                      <input type="submit" class="btn btn-success" value="Accès"><br/><br/>
                      <!--MDP: <b>-->
                        <?php
                           // echo htmlspecialchars(substr($session_link,-4));
                        ?>
                      <!-- </b>-->
                    </div>
                </div>
                <!-- -->
              </div>
          </div>
        </div>
    </form>
    <!-- -->
    <br/>
</body>
</html>
