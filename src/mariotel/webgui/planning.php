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

// ---
require_once "config.php";
// ---

define('UNCHECKED', '');
define('CHECKED', 'checked');
// ---
$past_sessions      = (empty($_SESSION["filters"]["past_sessions"]))      ? UNCHECKED : ($_SESSION["filters"]["past_sessions"]);
$same_user_sessions = (empty($_SESSION["filters"]["same_user_sessions"])) ? UNCHECKED : ($_SESSION["filters"]["same_user_sessions"]);
$this_week_sessions = (empty($_SESSION["filters"]["this_week_sessions"])) ? UNCHECKED : ($_SESSION["filters"]["this_week_sessions"]);
// ---
// $this_week = date("YW");  // Ex: 202042
// Read filters:
if ($_SERVER["REQUEST_METHOD"] == "POST") {
    // ---
    $past_sessions      = (empty($_POST["past_sessions"]))      ? UNCHECKED : CHECKED;
    $same_user_sessions = (empty($_POST["same_user_sessions"])) ? UNCHECKED : CHECKED;
    $this_week_sessions = (empty($_POST["this_week_sessions"])) ? UNCHECKED : CHECKED;
    // ---
    // Store data in session variables:
    $_SESSION["filters"]["past_sessions"] = $past_sessions;
    $_SESSION["filters"]["same_user_sessions"] = $same_user_sessions;
    $_SESSION["filters"]["this_week_sessions"] = $this_week_sessions;
    // ---
}

// SQL condition built according to filters:
$sql_cond_past_sessions      = ($past_sessions      == UNCHECKED) ? ["(date(starts) >= CURDATE())"] : [];
$sql_cond_same_user_sessions = ($same_user_sessions == UNCHECKED) ? [] : ["(username = '".$_SESSION["username"]."')"];
$sql_cond_this_week_sessions = ($this_week_sessions == UNCHECKED) ? [] : ["(YEARWEEK(starts, 3) = '".date("YW")."')"];
$sql_cond = implode(' && ', array_merge(["TRUE"], $sql_cond_past_sessions, $sql_cond_same_user_sessions, $sql_cond_this_week_sessions));
// ---
// Note: PHP date() follows the ISO 8601 specification, while MYSQL YEARWEEK() needs the additional argument 3 to run in this mode.
// Thanks to Mark Reed for the explanation (https://stackoverflow.com/questions/15562270/php-datew-vs-mysql-yearweeknow).
// ---
// Declaration:
$num_rows = 0;
// ---
$sql = "SELECT * FROM sessions WHERE ".$sql_cond." ORDER BY starts";
// --
if ($sql_result = mysqli_query($link, $sql)) {
    // --
    $num_rows = mysqli_num_rows($sql_result);
    // --
} else {
    // echo "ERROR: Could not able to execute $sql. " . mysqli_error($link);
    header("location: error.php?noway=p0");
    exit;
}

// $sql_result will be used later in the code

function echo_sessions_de_user() {
  $username = $_SESSION["username"];
  if (strlen($username) <= 12) {
    echo 'De <b>'.$username.'</b>';
  } else {
    echo '<b>'.$username.'</b>';
  }
}

?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Planning</title>
    <!-- -->
    <script type="text/javascript" src="/node_modules/jquery/dist/jquery.min.js"></script>
    <script type="text/javascript" src="/node_modules/bootstrap/dist/js/bootstrap.min.js"></script>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .wrapper{ width: 1050px; margin: 0 auto; }
        .page-header h2{ margin-top: 0; }
        table tr td:last-child a{ margin-right: 5px; }
        th{ text-align: center; }
        body{ font: 14px sans-serif; text-align: center; }
        .glyphicon.glyphicon-info-sign { font-size: 20px; }
        .session_link { font-family: "Courier New", Courier, "Lucida Sans Typewriter", "Lucida Typewriter", monospace;
                        font-size: 11px; font-style: normal; font-variant: normal; }
        .form-check-label{ font: 12px sans-serif; text-align: left; }
    </style>
    <script type="text/javascript">
        $(document).ready(function(){
            $('[data-toggle="tooltip"]').tooltip();
        });
    </script>
</head>
<body>
    <div class="page-header">
        <h2>Planning des salles Mariotel<?php echo_info_anchor(); ?></h2>
    </div>
    <!-- DEBUGGING: <?php echo "SQL QUERY: ".$sql; ?> -->
    <br/>
    <!-- -->
    <?php make_toolbar_when($num_rows >= 20, True, False); ?>
    <!-- -->
    <div class="wrapper">
      <div class="container-fluid">

        <div class="row">
          <div class="col-md-10">
            <div class="page-header clearfix">
                <h3 class="pull-left">Réservations planifiées <small>(<?php echo $num_rows;?>)</small></h3>
            </div>
          </div> <!--col-->
          <div class="col-md-2">
            <div class="page-header clearfix">
                <!-- -->
                <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post">
                    <div class="pull-left">
                      <input type="submit" class="btn btn-link" value="Filtres" title="Appliquer les filtres sélectionnés">
                    </div>
                    <br><br>
                    <!-- -->
                    <div class="pull-right">
                      <div class="form-check" title="Seulement les sessions de l'utilisateur courant">
                          <label class="form-check-label" for="same_user_sessions"><?php echo_sessions_de_user();?></label>&nbsp;
                          <input type="checkbox" class="form-check-input" id="same_user_sessions" name="same_user_sessions" <?php echo $same_user_sessions; ?>>
                          &nbsp;&nbsp;
                      </div>
                    </div>
                    <!-- -->
                    <div class="pull-right">
                      <div class="form-check" title="Seulement les sessions de cette semaine">
                          <label class="form-check-label" for="this_week_sessions">Cette semaine <b><?php echo date("W");?></b></label>&nbsp;
                          <input type="checkbox" class="form-check-input" id="this_week_sessions" name="this_week_sessions" <?php echo $this_week_sessions; ?>>
                          &nbsp;&nbsp;
                      </div>
                    </div>
                    <!-- -->
                    <div class="pull-right">
                      <div class="form-check" title="Montrer les sessions des jours et semaines précédentes">
                          <label class="form-check-label" for="past_sessions">Inclure révolues</label>&nbsp;
                          <input type="checkbox" class="form-check-input" id="past_sessions" name="past_sessions" <?php echo $past_sessions; ?>>
                          &nbsp;&nbsp;
                      </div>
                    </div>
                    <!-- -->
                </form>
                <!-- -->
            </div> <!--page-header-->

          </div> <!--col-->
        </div> <!--row -->

        <div class="row">
          <div class="col-md-12">
            <?php
            // --
            if ($num_rows > 0) {
                echo "<table class='table table-bordered table-striped'>";
                    echo "<thead>";
                        echo "<tr>";
                            echo "<th><small class='text-muted'>#</small></th>";
                            echo "<th>Date</th>";
                            echo "<th>Début</th>";
                            echo "<th>Fin</th>";
                            echo "<th>Durée</th>";
                            echo "<th>Étudiants</th>";
                            echo "<th>Responsable</th>";
                            echo "<th>Lien</th>";
                            echo "<th>Ports</th>";
                            echo "<th>État</th>";
                            echo "<th>Action</th>";
                        echo "</tr>";
                    echo "</thead>";
                    echo "<tbody>";
                    // ---
                    $starts_week_pred = "";
                    // ---
                    while($row = mysqli_fetch_array($sql_result)){
                        $d0 = new DateTime($row['starts']);
                        $d1 = new DateTime($row['finish']);
                        // $starts = date ('Y-m-d H:i', $d0->getTimestamp());  // Ex: 2020-07-04 15:00
                        $starts_day  = date ('d-m-Y', $d0->getTimestamp());    // Ex: 04-07-2020
                        $starts_time = date ('H:i'  , $d0->getTimestamp());    // Ex: 15:00
                        $starts_week = date ('W',     $d0->getTimestamp());    // weak number in the year
                        $finish_time = date ('H:i'  , $d1->getTimestamp());    // Ex: 18:00
                        // ---
                        $_SESSION["mariotel_sessions_cache"][$row['id']] = $row;
                        // ---
                        if ($starts_week !== $starts_week_pred) {
                            // ---
                            echo "<tr style=\"background-color:Gainsboro;\"><td colspan=\"11\"><b><small>Semaine ".$starts_week."</small></b></td></tr>";
                            $starts_week_pred = $starts_week;
                            // ---
                            }
                        // ---
                        echo "<tr>";
                            echo "<td><small class='text-muted'>" . $row['id'] . "</small></td>";
                            echo "<td>" . $starts_day           . "</td>";
                            echo "<td>" . $starts_time          . "</td>";
                            echo "<td>" . $finish_time          . "</td>";
                            echo "<td>" . ($row['duration']/60) . "</td>";
                            echo "<td>" . $row['student_nb']    . "</td>";
                            echo "<td>" . $row['username']      . "</td>";
                            // ---
                            echo '<td><div class="session_link">' . teacher_anchor_of_session_link($row['link'],$row['link']) . "</div></td>";
                            // ---
                            echo "<td><small class='text-muted'>" . $row['first_port'] . "</small></td>";
                            echo "<td>";
                            if ($row['status'] ==='running')  { echo "<span class='glyphicon glyphicon-cog' title='En cours'></span></a>"; } else {
                            if ($row['status'] ==='anomaly')  { echo "<span class='glyphicon glyphicon-warning-sign' title='Anomalie'></span></a>"; } else {
                            if ($row['status'] ==='finished') { echo "<span class='glyphicon glyphicon-ok' title='Terminée'></span></a>"; }}}
                            echo "</td>";
                            echo "<td>";
                            // ---
                            //    echo "<a href='update.php?id=". $row['id'] .
                            //      "' title='Modifier' data-toggle='tooltip'><span class='glyphicon glyphicon-pencil'></span></a>";
                            // ---
                            if ($_SESSION["username"] === $row['username'] || $_SESSION["admin_level"] >= 1) {
                                // ---
                                echo "<a href='details.php?id=". $row['id'] .
                                    "' title='Détails' data-toggle='tooltip'><span class='glyphicon glyphicon-eye-open'></span></a>";
                                // ---
                                }
                            // ---
                            if (($_SESSION["username"] === $row['username'] && $row['status'] ==='planned') || ($_SESSION["admin_level"] >= 2)) {
                                // ---
                                echo "<a href='delete.php?id=". $row['id'] .
                                  "' title='Supprimer' data-toggle='tooltip'><span class='glyphicon glyphicon-trash'></span></a>";
                                // ---
                                }
                            // ---
                            echo "</td>";
                        echo "</tr>";
                    }
                    echo "</tbody>";
                echo "</table>";
                // Free result set
                mysqli_free_result($sql_result);
            } else {
                echo "<p class='lead'><em>Aucune réservation</em></p>";
            } // ($num_rows > 0)
            ?>
          </div>
        </div>
      </div>
      <hr>
    </div>
    <!--  -->
    <?php make_toolbar_when(True, True, False); ?>
    <br/><br/>
    <!--  -->
</body>
</html>
