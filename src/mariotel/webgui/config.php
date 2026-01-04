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

// mysql> USE mariotel;
//
// mysql> DESCRIBE users;
// +-------------+--------------+------+-----+-------------------+-------------------+
// | Field       | Type         | Null | Key | Default           | Extra             |
// +-------------+--------------+------+-----+-------------------+-------------------+
// | id          | int          | NO   | PRI | NULL              | auto_increment    |
// | username    | varchar(50)  | NO   | UNI | NULL              |                   |
// | password    | varchar(255) | NO   |     | NULL              |                   |
// | email       | varchar(255) | YES  |     | NULL              |                   |
// | created_by  | varchar(50)  | YES  |     | NULL              |                   |
// | created_at  | datetime     | YES  |     | CURRENT_TIMESTAMP | DEFAULT_GENERATED |
// | admin_level | int          | NO   |     | 0                 |                   |
// +-------------+--------------+------+-----+-------------------+-------------------+
// 7 rows in set (0.00 sec)
//
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
//

// The following test allows other PHP scripts to include this script (i.e. config.php)
// preventing the connection to the database:
if (!(isset($PREVENT_DATABASE_CONNECTION))) {

  /* Database credentials. Assuming you are running MySQL
  server with default setting (user 'root' with no password) */
  define('DB_SERVER',   'localhost');
  define('DB_USERNAME', 'mariotel');
  define('DB_PASSWORD', 'MariotelMysql2020!!');
  define('DB_NAME',     'mariotel');

  /* Attempt to connect to MySQL database */
  $link = mysqli_connect(DB_SERVER, DB_USERNAME, DB_PASSWORD, DB_NAME);

  // Check connection
  if($link === false){
      die("ERROR: Could not connect. " . mysqli_connect_error());
  }

}

// Defaults:
$NO_VNC_STARTING_PORT=26900;
setlocale (LC_TIME, 'fr_FR.utf8', 'fra'); // pour le jour de la semaine en français
date_default_timezone_set('Europe/Paris');

// Some pervasives tools:

$french_session_status =
  array("planned"=>"en attente", "running"=>"en cours", "finished"=>"terminée", "anomaly"=>"anomalie");

function student_anchor_of_session_link($session_link, $anchor_content="") {
  // $protocol = apache_getenv('HTTPS') ? 'https://' : 'http://';
  // $protocol = stripos($_SERVER['SERVER_PROTOCOL'],'https') === 0 ? 'https://' : 'http://';
  $protocol = (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] == 'on') ? 'https://' : 'http://';
  $href= $protocol . $_SERVER["HTTP_HOST"]. "/session.php?link=" . $session_link;
  if(empty($anchor_content)) { $anchor_content = htmlspecialchars($href); }
  // ---
  $anchor='<a target="_blank" href="' . $href . '">' . $anchor_content . '</a>';
  // ---
  return $anchor;
}

function teacher_anchor_of_session_link($session_link, $anchor_content="") {
  // $protocol = apache_getenv('HTTPS') ? 'https://' : 'http://';
  // $protocol = stripos($_SERVER['SERVER_PROTOCOL'],'https') === 0 ? 'https://' : 'http://';
  $protocol = (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] == 'on') ? 'https://' : 'http://';
  $teacher_key = strrev( substr( sha1( strrev(substr($session_link, -11, 7))), 3, 10)); // an arbitrary annoying formula
  $href= $protocol . $_SERVER["HTTP_HOST"]. "/session.php?teacher=" . $teacher_key . "&link=" . $session_link;
  if(empty($anchor_content)) { $anchor_content = htmlspecialchars($href); }
  // ---
  $anchor='<a target="_blank" href="' . $href . '">' . $anchor_content . '</a>';
  // ---
  return $anchor;
}

function formatted_student_name($name) {
  // ---
  $nice_name = strtolower(trim($name));
  $nice_name = ucwords($nice_name);
  // ---
  return $nice_name;
}

function echo_info_anchor() {
  echo '<a href="info.php" title="Informations sur le projet Mariotel" data-toggle="tooltip">';
  echo '<sup><span class="glyphicon glyphicon-info-sign"></span></sup>';
  echo '</a>';
}

// Example:
// ---
// $row = NULL;
// $SUCCESS = simple_sql_one_answer_query("SELECT id, username, password, email, admin_level FROM users WHERE username = ?", "s", $username, $row);
// $email = $row["email"];
// ---
function simple_sql_one_answer_query($sql, $format, $arg, &$row) {
  // ---
  global $link; // defined above in this file
  // ---
  $SUCCESS = false;
  // ---
  if($stmt = mysqli_prepare($link, $sql)) {
      // Bind variables to the prepared statement as parameters
      mysqli_stmt_bind_param($stmt, $format, $param);

      // Set parameters
      $param = $arg;

      // Attempt to execute the prepared statement
      if(mysqli_stmt_execute($stmt)){
          $result = mysqli_stmt_get_result($stmt);

          if(mysqli_num_rows($result) == 1){
              /* Fetch result row as an associative array. Since the result set
              contains only one row, we don't need to use while loop */
              $row = mysqli_fetch_array($result, MYSQLI_ASSOC);
              $SUCCESS = true;
          }
      }

  // Close statement
  mysqli_stmt_close($stmt);

  } // mysqli_prepare
  // ---
  return $SUCCESS;
  // ---
} // simple_sql_one_answer_query


// Example:
// ---
// $SUCCESS = simple_sql_statement("DELETE FROM sessions WHERE id = ?", "i", $id);
// ---
function simple_sql_statement($sql, $format, $arg) {
  // ---
  global $link; // defined above in this file
  // ---
  $SUCCESS = false;
  // ---
  if($stmt = mysqli_prepare($link, $sql)) {
      // Bind variables to the prepared statement as parameters
      mysqli_stmt_bind_param($stmt, $format, $param);

      // Set parameters
      $param = $arg;

      // Attempt to execute the prepared statement
      if(mysqli_stmt_execute($stmt)){
          $SUCCESS = true;
      }

  // Close statement
  mysqli_stmt_close($stmt);

  } // mysqli_prepare
  // ---
  return $SUCCESS;
  // ---
} // simple_sql_statement

//  -----------   --------   ------   -----------   --------
// |Déconnexion| |Inscrire| |Profil| |Réservation| |Planning|
//  -----------   --------   ------   -----------   --------
function make_toolbar_when($condition, $reservation=True, $planning=True) {
  // ---
  if ($condition) {
    // ---
    // Provide (or not) the button to add a new user:
    $register_button="";
    if ((isset($_SESSION["admin_level"])) && ($_SESSION["admin_level"] > 0)){
      $register_button='<a href="register.php" class="btn btn-default" title="Ajouter un nouvel utilisateur (enseignant)">Inscrire</a>&nbsp;&nbsp;';
    }
    // ---
    // Provide (or not) the button "Réservations":
    $reservation_button="";
    if ($reservation){
      $reservation_button='<a href="booking.php" class="btn btn-primary" title="Aller à la page de saisie des réservations">Réservation</a>&nbsp;&nbsp;';
    }
    // ---
    // Provide (or not) the button "Planning":
    $planning_button="";
    if ($planning){
      $planning_button='<a href="planning.php" class="btn btn-primary" title="Aller à la page des réservations effectuées">Planning</a>&nbsp;&nbsp;';
    }
    // ---
    echo '
    <div class="container" id="buttons">
      <p>
        <a href="logout.php" class="btn btn-danger">Déconnexion</a>&nbsp;&nbsp;'.
        $register_button.'
        <a href="profile.php" class="btn btn-warning" title="Modifier son profil (email, mot de passe, ..)">Profil</a>&nbsp;&nbsp;'.
        $reservation_button.
        $planning_button.'
      </p>
    </div>
    ';
    }
}

// Ex: $x='77.66.55.44'
function is_ipv4_address($x) {
  // ---
  if (filter_var($x, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4 | FILTER_FLAG_NO_RES_RANGE)) { return True; } else { return False; }
  // ---
}

// Ex: $x='11.12.13.14'
function is_ipv4_public_address($x) {
  // ---
  if (filter_var($x, FILTER_VALIDATE_IP, FILTER_FLAG_IPV4 | FILTER_FLAG_NO_PRIV_RANGE | FILTER_FLAG_NO_RES_RANGE)) { return True; } else { return False; }
  // ---
}

// Ex: $x='192.168.0.1'
function is_ipv4_private_address($x) {
  // ---
  return (is_ipv4_address($x) && (! is_ipv4_public_address($x)));
  // ---
}

// ---
// Geographical vs political country classification:
$real_country = [
    # France related bindings:
    "Guadeloupe" => "France",
    "Guiana"     => "France",
    "Guyana"     => "France",
    "Martinique" => "France",
    "Mayotte"    => "France",
    "Reunion"    => "France",
    # Add here the other relevant bindings for your server:
    # ...
];
// ---
function translate_country_or_leave_unchanged($x) {
  global $real_country;
  # ---
  if (array_key_exists($x, $real_country)) { return $real_country[$x]; } else { return $x; }
  # ---
}
// ---
function geoiplookup_wrapper($x) {
  # ---
  $country = trim(shell_exec("geoiplookup ".$x." | awk '{print \$NF}'"));
  return ($country);
}
// ---
// Examples:
//    var_dump(test_are_they_in_the_same_country("109.203.232.1","109.9.108.67"));
//    if (test_are_they_in_the_same_country($_SERVER["SERVER_ADDR"], $_SERVER["REMOTE_ADDR"])) { ... }
function test_are_they_in_the_same_country($x, $y) {
  # ---
  $country_x = geoiplookup_wrapper($x);
  $country_y = geoiplookup_wrapper($y);
  # ---
  $country_x = translate_country_or_leave_unchanged($country_x);
  $country_y = translate_country_or_leave_unchanged($country_y);
  # ---
  return ($country_x == $country_y);
}

// Example:
// if (test_reverse_ipv4_lookup('176.148.125.1')) { echo "YES"; }
function test_reverse_ipv4_lookup($x) {
  // ---
  return (! ($x == gethostbyaddr($x)));
  // ---
}

function test_reverse_ipv4_lookup_or_same_country($x) {
  // ---
  return ((test_reverse_ipv4_lookup($x)) || (test_are_they_in_the_same_country($x, $_SERVER["SERVER_ADDR"])));
  // ---
}

function test_reverse_ipv4_lookup_or_same_country_or_private($x) {
  // ---
  return ((test_reverse_ipv4_lookup_or_same_country($x)) || (is_ipv4_private_address($x)));
  // ---
}

// Example:
//   if (get_reverse_ipv4_lookup('194.254.163.100', $host)) { echo $host; }
function get_reverse_ipv4_lookup($x, &$host) {
  // ---
  $host = strtolower(gethostbyaddr($x));
  return (! ($x == $host));
  // ---
}

?>

