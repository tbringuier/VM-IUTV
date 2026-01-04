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

$session_link = trim($_GET["link"]);

// Check existence of `link' parameter:
if(empty($session_link)){
    // URL doesn't contain link parameter. Redirect to error page:
    header("location: error.php?noway=s0");
    exit();
}

// ---
$teacher_key = trim($_GET["teacher"]);
$expected_teacher_key = strrev( substr( sha1( strrev(substr($session_link,-11, 7))), 3, 10)); // an arbitrary annoying formula
// ---
if((! empty($teacher_key)) && (! ($teacher_key == $expected_teacher_key))) {
    // teacher_key not as expected, so:
    header("location: error.php?noway=s1");
    exit();
}

// Initialize the session:
session_start();

// Global boolean value:
$teacher_role = (! empty($teacher_key));

// Debugging:
ini_set('display_errors', 'on');

// Include config file
require_once "config.php";

// Only clients with reversible IP address may access (also teachers!).
// We accept also IP localized in the same country or private IP (same organization):
if (! test_reverse_ipv4_lookup_or_same_country_or_private($_SERVER["REMOTE_ADDR"])) {
    // ---
    header("location: error.php?noway=s2");
    exit();
}

// ---

// Prepare the select statement:
$sql = "SELECT * FROM sessions WHERE link = '" . $session_link . "'";
// --
if(!(($result = mysqli_query($link, $sql)) && (mysqli_num_rows($result) === 1) && ($row = mysqli_fetch_array($result)))) {
    // Strange user link:
    header("location: error.php?noway=s3");
    exit();
}
// var_dump($row);

// Some useful definitions:
$d0 = new DateTime($row['starts']);
$d1 = new DateTime($row['finish']);
// $starts = date ('Y-m-d H:i', $d0->getTimestamp());  // Ex: 2020-07-04 15:00
$starts_day  = date ('d-m-Y', $d0->getTimestamp());    // Ex: 04-07-2020
$starts_time = date ('H:i'  , $d0->getTimestamp());    // Ex: 15:00
$finish_time = date ('H:i'  , $d1->getTimestamp());    // Ex: 18:00
// ---
$minutes_to_start  = ($d0->getTimestamp() - time()) / 60;
$minutes_to_finish = ($d1->getTimestamp() - time()) / 60;
// ---
$starts_day_name = date ('l', $d0->getTimestamp());    // Ex: monday
$starts_week_nb  = date ('W', $d0->getTimestamp());    // Ex: 42
$starts_day_name_locale = strftime("%A", strtotime("$starts_day"));  // Ex: lundi
// ---
// Global boolean values:
$running_session = ($row['status'] == "running");
$planned_session = ($row['status'] == "planned");
// ---

// Reload the page each 15 seconds when the session is planned:
if ($planned_session || ($running_session && $teacher_role)) { $body_refresh_option=" onload='setTimeout(\"location.reload(true);\", 15000);'"; }
  else { $body_refresh_option="";}

// ---

$time_warning=""; // default
if ($planned_session && ($minutes_to_start>0) && ($minutes_to_start<20)) {
 $time_warning = sprintf("<h5>(début dans %d minutes)</h5>", $minutes_to_start);
}
// ---
elseif ($running_session && ($minutes_to_finish>=0) && ($minutes_to_finish<20)) {
 $time_warning = sprintf("<h5><b><p class='alert alert-warning' role='alert'>Attention : fin dans %d minutes</p></b></h5>", $minutes_to_finish);
}
// ---
elseif ($running_session && ($minutes_to_finish<0)) {
 $time_warning = sprintf("<h5><b><p class='alert alert-danger' role='alert'>SESSION TERMINÉE. Arrêt définitif des stations dans %d minutes</p></b></h5>", ($minutes_to_finish + 10 + 1));
}

// ---

// Initialize the array of workstation's assignements:
$workstations = array_fill(0, 1 + $row['student_nb'] + 4, ""); // 4 is the max number of unused stations
$busy_workstations_nb = 0;

// When the session is running or terminated, it's became relevant
// for the teacher to control the list of connected students:
if (!($row['status'] == "planned")) {
  // Query now the table `workstations':
  $sql = "SELECT * FROM workstations WHERE workstation>0 AND link = '" . $session_link . "'";
  // --
  if ($result = mysqli_query($link, $sql)) {
    // Fill the dictionary $workstations
    while ($ws = mysqli_fetch_array($result)) {
      $key = $ws['workstation'];
      $val = $ws['allowed_user'];
      $workstations[$key] = $val;
      $busy_workstations_nb++;
      }
    } // result ok
  // ---
} // status is running

if(($running_session) && (! $teacher_role)) {

    // Initialize the student session:
    session_start();

    // Store data in session variables, in order to continue
    // with all information retrieved or computed here:
    $_SESSION["row"] = $row;
    $_SESSION["GLOBALS"] = $GLOBALS;

    // students must provide their name:
    header("location: workstation.php");
    exit();
}

// Continue with ($teacher_role == true):
// ---
if ($running_session && $teacher_role) {
    // Register also the teacher:
    $sql = "INSERT INTO workstations (link, workstation, allowed_user, remote_socket) VALUES (?, ?, ?, ?)";
    $stmt = mysqli_prepare($link, $sql);
    // ---
    $WORKSTATION_INDEX = 0; // the teacher has the workstation 0
    $teacher_name = $row['username'];
    $remote_socket = $_SERVER["REMOTE_ADDR"] . ":" . $_SERVER["REMOTE_PORT"];
    // ---
    // Bind variables to the prepared statement as parameters:
    mysqli_stmt_bind_param($stmt, "siss", $session_link, $WORKSTATION_INDEX, $teacher_name, $remote_socket);
    // ---
    // Attempt to execute the prepared statement (ignoring errors occurring
    // when the same record has already been inserted):
    if (mysqli_stmt_execute($stmt)){ $FOUND_FREE = True; } else {
      $FOUND_FREE = False;
      // TODO: the IP address should be the same (to be checked)
      }

    // Close statement
    mysqli_stmt_close($stmt);
}

// Close connection:
mysqli_close($link);

// ---
// Ex: http://precision:26902/vnc.html
// $workstation_index starts from 1 for students (0 is the teacher index)
// ---
function make_a_link_to_virtual_machine($first_port, $workstation_index, $row) {
  // ---
  $passwd = ($workstation_index == 0) ? substr($row['link'],-6) : substr($row['link'],-4);
  // ---
  if (($workstation_index <= $row['student_nb']) && ($row['status'] == "running")) {
    $img='<img src="images/ico.machine.on.med.png">';
    $port=$first_port + $workstation_index;
    // $anchor='<a href="https://'.$_SERVER['SERVER_NAME'].':'.$port.'/'.'vnc.html?debug=1&password='.$passwd.'" target="_blank">'.$img.'</a>';
    $anchor='<a href="http://'.$_SERVER['SERVER_NAME'].':'.$port.'/'.'vnc.html?debug=1&password='.$passwd.'" target="_blank">'.$img.'</a>';
    }
  else {
    $img='<img src="images/ico.machine.off.med.png">';
    $anchor='<a href="">'.$img.'</a>';
    }
  // ---
  return $anchor;
}

// empty cell without borders:
function empty_no_borders_td() {
  return '<td style="border-top: none; border-bottom: none;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>';
}

function empty_no_borders_th() {
  return '<th style="border-top: none; border-bottom: none;">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</th>';
}

// Format the name of the student:
// wordwrap(mb_strimwidth(ucwords(strtolower($workstations[$i])), 0, 24, "..."), 12, "<br/>\n");
function workstation_index_with_student_name_td($i) {
  global $workstations;
  // ---
  $nice_name = formatted_student_name($workstations[$i]);
  $nice_name = mb_strimwidth($nice_name, 0, 24, "...");
  $nice_name = wordwrap($nice_name, 12, "<br/>\n");
  // ---
  $result = "<td>" . $i . "<br/><em><mark>" . $nice_name  . "</em></mark></td>";
  // ---
  return $result;
}

?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Session Mariotel</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .wrapper{ width: 1000px; margin: 0 auto; }
        .wrapper_ens{ width: 250px; margin: 0 auto; }
        .page-header h2{ margin-top: 0; }
        table tr td:last-child a{ margin-right: 11px; }
        th{ text-align: center; }
        body{ font: 14px sans-serif; text-align: center; }
    </style>

</head>
<body <?php echo $body_refresh_option;?>>

    <div class="page-header">
        <h2>Accès à la salle virtuelle Mariotel</h2>
        <h6><?php echo $row['link'];?></h6>
        <h5>Réservée par <b><?php echo $row['username']; ?></b>, <?php echo $row['student_nb'];?> postes,
            <b><?php echo $french_session_status[$row['status']];?></b></h5>
        <h5>Pour la semaine <?php echo $starts_week_nb.", <b>".$starts_day_name_locale." ".$starts_day."</b>";?>
        de <?php echo $starts_time;?> à <?php echo $finish_time;?> </h5>
        <?php echo $time_warning;?>
    </div>
    <div class="wrapper">
      <div class="container-fluid">
        <div class="row">
          <div class="col-md-12">
            <!-- <div class="page-header clearfix"> -->
            <div class="page-header">
                <h3 class="pull-center">Affectation des stations de travail <small class='text-muted'>(<?php echo $busy_workstations_nb."/".$row['student_nb'];?>)</small></h3>
            </div> <!-- header -->
            <!-- -->
            <?php
              echo "<table class='table table-bordered table-striped'>";
              //echo "<table class='table table-striped'>";
                  echo "<thead>";
                      echo "<tr>";
                          echo '<th>Étudiant</th>';
                          echo "<th>Station</th>";
                          echo empty_no_borders_th();
                          echo "<th>Étudiant</th>";
                          echo "<th>Station</th>";
                          echo empty_no_borders_th();
                          echo "<th>Étudiant</th>";
                          echo "<th>Station</th>";
                          echo empty_no_borders_th();
                          echo "<th>Étudiant</th>";
                          echo "<th>Station</th>";
                          echo empty_no_borders_th();
                          echo "<th>Étudiant</th>";
                          echo "<th>Station</th>";
                          // echo "<th>État</th>";
                      echo "</tr>";
                  echo "</thead>";
                  echo "<tbody>";
                      // ---
                      $i=0;
                      while($i < $row['student_nb']){
                          echo "<tr>";
                              // ---
                              $i++;
                              echo workstation_index_with_student_name_td($i);
                              echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], $i, $row) . "</td>";
                              echo empty_no_borders_td();
                              // ---
                              $i++;
                              echo workstation_index_with_student_name_td($i);
                              echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], $i, $row) . "</td>";
                              echo empty_no_borders_td();
                              // ---
                              $i++;
                              echo workstation_index_with_student_name_td($i);
                              echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], $i, $row) . "</td>";
                              echo empty_no_borders_td();
                              // ---
                              $i++;
                              echo workstation_index_with_student_name_td($i);
                              echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], $i, $row) . "</td>";
                              echo empty_no_borders_td();
                              // ---
                              $i++;
                              echo workstation_index_with_student_name_td($i);
                              echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], $i, $row) . "</td>";
                          // ---
                          echo "</tr>";
                          echo "<tr height = 25px></tr>";
                      }
                  echo "</tbody>";
              echo "</table>";
            ?>
            <!-- -->
          </div>
        </div>
        <!-- -->
        <!-- -->
      </div>
    </div>
    <!-- -->
    <div class="wrapper_ens">
      <div class="container-fluid">
        <div class="row">
          <div class="col-md-12">
            <div class="page-header">
                <h3 class="pull-center">Poste enseignant</h3>
            </div> <!-- header -->
            <!-- -->
            <?php
              echo "<table class='table table-bordered table-striped'>";
                  echo "<tbody>";
                      // ---
                      echo "<tr>";
                          // ---
                          echo "<td>" . $row['username']  . "</td>";
                          echo "<td>" . make_a_link_to_virtual_machine($row['first_port'], 0, $row) . "</td>";
                      // ---
                      echo "</tr>";
                  echo "</tbody>";
              echo "</table>";
            ?>
            <!-- -->
          </div>
        </div>
      </div>
    </div>
    <!--  -->
</body>
</html>
