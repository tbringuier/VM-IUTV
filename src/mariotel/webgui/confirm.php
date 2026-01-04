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

if(!isset($_SESSION["booking"])){
    header("location: booking.php");
    exit;
}

if(isset($_SESSION["booking"]["booked"]) && $_SESSION["booking"]["booked"] === true){
  header("location: booking.php");
  exit;
}

// Include config file:
require_once "config.php";

date_default_timezone_set('Europe/Paris');

// Usage:
//   $myRandomString = generateRandomString(10);
function generate_random_string($length = 25) {
  $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $charactersLength = strlen($characters);
  $randomString = '';
  for ($i = 0; $i < $length; $i++) { $randomString .= $characters[rand(0, $charactersLength - 1)]; }
  return $randomString;
}

// SELECT max(first_port) FROM sessions WHERE NOT (
//   (DATE_ADD(finish, INTERVAL 10 MINUTE) < DATE_ADD('2020-07-30 09:00', INTERVAL -5 MINUTE)) OR
//   (DATE_ADD('2020-07-30 12:00', INTERVAL 10 MINUTE) < DATE_ADD(starts, INTERVAL -5 MINUTE))
//   )
function get_free_port_number($link, $starts, $finish) {
  // Prepare an insert statement
  $sql = "SELECT max(first_port) AS max_conflicting_port FROM sessions WHERE
   NOT (
    (DATE_ADD(finish, INTERVAL 10 MINUTE) < DATE_ADD(   ?  , INTERVAL -5 MINUTE)) OR
    (DATE_ADD(  ?   , INTERVAL 10 MINUTE) < DATE_ADD(starts, INTERVAL -5 MINUTE))
    )";
  // ---
  if($stmt = mysqli_prepare($link, $sql)){
      // Bind variables to the prepared statement as parameters
      mysqli_stmt_bind_param($stmt, "ss", $starts, $finish);
      // Attempt to execute the prepared statement
      if(!mysqli_stmt_execute($stmt)){
          echo "Something went wrong (1). Please try again later.";
          mysqli_stmt_close($stmt);
          header("location: booking_error.php");
      }
    // ---
    $stmt->bind_result($max_conflicting_port); // Lecture des variables résultantes
    $stmt->fetch();                            // Récupération des valeurs
    $stmt->close();                            // Fermeture du traitement
    }
  // ---
  if(!isset($max_conflicting_port)){
      // Defined in config.php
      $result=$GLOBALS['NO_VNC_STARTING_PORT'];
  } else {
      $result = $max_conflicting_port + 100;
  }
  // ---
  return $result;
}

// Get the whole array:
$booking = $_SESSION["booking"];
// ---
$param_starts       = $booking["date_time"];
$param_duration     = $booking["duration"] * 60;
$param_username     = $_SESSION["username"];
$param_email        = $booking["email"];
$param_student_nb   = $booking["student_nb"];
//$param_student_list = $booking["student_list"];
$param_student_list = "";
// ---
$random_string  = generate_random_string(16);
$param_status   = "planned";
// ---
$d0 = new DateTime($param_starts);    // Ex: 2020-07-04 15:00:00.000000
$d1 = new DateTime($param_starts);
$duration_minutes = $param_duration;  // Ex: 2.5
$diff_minutes = new DateInterval('PT'.$duration_minutes.'M');
$d1->add($diff_minutes);              // Ex: 2020-07-04 17:30:00.000000
// ---
$param_starts = date ('Y-m-d H:i', $d0->getTimestamp());  // Ex: 2020-07-04 15:00
$param_finish = date ('Y-m-d H:i', $d1->getTimestamp());  // Ex: 2020-07-04 17:30
// ---
$param_link = $param_username . '-' . str_replace(array(' ','-',':'), '', $param_starts) . '-' . $random_string;
// Store data in session variables:
$_SESSION["booking"]["link"] = $param_link;

// $param_first_port = 26900; // probably...
$param_first_port = get_free_port_number($link, $param_starts, $param_finish);

// ---
// mysql> DESCRIBE sessions ;
// +--------------+--------------+------+-----+-------------------+-------------------+
// | Field        | Type         | Null | Key | Default           | Extra             |
// +--------------+--------------+------+-----+-------------------+-------------------+
// | id           | int          | NO   | PRI | NULL              | auto_increment    |
// | starts       | datetime     | NO   |     | NULL              |                   |
// | finish       | datetime     | NO   |     | NULL              |                   |
// | duration     | int          | NO   |     | NULL              |                   |
// | link         | varchar(255) | NO   | UNI | NULL              |                   |
// | status       | varchar(50)  | NO   |     | NULL              |                   |
// | username     | varchar(50)  | NO   | MUL | NULL              |                   |
// | email        | varchar(255) | YES  |     | NULL              |                   |
// | student_nb   | int          | NO   |     | NULL              |                   |
// | student_list | text         | YES  |     | NULL              |                   |
// | first_port   | int          | YES  |     | NULL              |                   |
// | created_at   | datetime     | YES  |     | CURRENT_TIMESTAMP | DEFAULT_GENERATED |
// +--------------+--------------+------+-----+-------------------+-------------------+
// 12 rows in set (0.01 sec)


// Prepare an insert statement:
$sql = "INSERT INTO sessions (starts, finish, duration, link, status, username, email, student_nb, student_list, first_port) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";

// ----------
if($stmt = mysqli_prepare($link, $sql)){

    // Bind variables to the prepared statement as parameters:
    mysqli_stmt_bind_param($stmt, "ssissssisi",
      $param_starts, $param_finish, $param_duration, $param_link, $param_status, $param_username, $param_email, $param_student_nb, $param_student_list, $param_first_port
      );

    // Attempt to execute the prepared statement:
    if(!mysqli_stmt_execute($stmt)){
        echo "Something went wrong (1). Please try again later.";
        mysqli_stmt_close($stmt);
        header("location: booking_error.php");
    }

    // Store data in session variables:
    $_SESSION["booking"]["booked"] = true;

    // Close statement
    mysqli_stmt_close($stmt);
}

// ----------
// Try to get an already registered place for the student:
$sql = "SELECT * FROM sessions WHERE " . "link = '" . $param_link . "'" . " AND " . "username = '" . $param_username . "'";
// --
if ((($result = mysqli_query($link, $sql)) && (mysqli_num_rows($result) === 1) && ($row = mysqli_fetch_array($result)))) {
  $_SESSION["mariotel_sessions_cache"][$row['id']] = $row;
  header("location: details.php?confirm=1&id=". $row['id']);
}
else {
  // Very strange case (because INSERT succeeded):
  header("location: booking_error.php");
}
// Close connection
// mysqli_close($link);

?>

