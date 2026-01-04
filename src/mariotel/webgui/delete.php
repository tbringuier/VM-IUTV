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

// Process delete operation after confirmation
if(isset($_POST["id"]) && !empty($_POST["id"])){
    // Include config file
    require_once "config.php";
    // ---
    $id = trim($_POST["id"]);
    // ---
    $row = NULL;
    $SUCCESS = simple_sql_one_answer_query("SELECT link FROM sessions WHERE id = ?", "i", $id, $row);
    if (! $SUCCESS) { header("location: error.php?noway=d0"); exit(); }
    // --- else continue:
    // ---
    $session_link = $row["link"];
    $SUCCESS = simple_sql_statement("DELETE FROM workstations WHERE link = ?", "s", $session_link);
    if (! $SUCCESS) { header("location: error.php?noway=d1"); exit(); }
    // --- else continue:
    // ---
    $SUCCESS = simple_sql_statement("DELETE FROM sessions WHERE id = ?", "i", $id);
    if (! $SUCCESS) { header("location: error.php?noway=d2"); exit(); }
    // ---
    // Records deleted successfully. Redirect to landing page:
    header("location: planning.php");
    exit();

} else {
    // Check existence of id parameter
    if(empty(trim($_GET["id"]))){
        // URL doesn't contain id parameter. Redirect to error page
        header("location: error.php?noway=d3");
        exit();
    }

    // Only the owner of the reservation can delete it, except administrators of level 2 at least:
    if(($_SESSION["admin_level"] <= 1) && ($_SESSION["username"] !== $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["username"])) {
        header("location: error.php?noway=d4");
        exit();
        }

    // Only not running reservations can be deleted:
    if($_SESSION["mariotel_sessions_cache"][$_GET["id"]]["status"] == 'running') {
        header("location: error.php?noway=d5");
        exit();
        }

}
?>
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Supprimer</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .wrapper{ width: 450px; margin: 0 auto; }
        .page-header h2{ margin-top: 0; }
        table tr td:last-child a{ margin-right: 11px; }
        body{ font: 14px sans-serif; text-align: center; }
    </style>

</head>
<body>
    <div class="page-header">
        <h2>Supprimer une séance</h2>
    </div>
    <br/>
    <!--  -->
    <!-- -->
    <div class="wrapper">
        <div class="container-fluid">
            <div class="row">
                <div class="col-md-12">
                    <div class="form-group">
                        <label>Début</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["starts"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Fin</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["finish"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Durée</label>
                        <p class="form-control-static"><?php echo ($_SESSION["mariotel_sessions_cache"][$_GET["id"]]["duration"]/60); ?></p>
                    </div>
                    <div class="form-group">
                        <label>Étudiants</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["student_nb"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Responsable</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["username"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Notification</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["email"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Réservation effectuée le</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["created_at"]; ?></p>
                    </div>
                    <div class="form-group">
                        <label>Lien</label>
                        <p class="form-control-static"><?php echo $_SESSION["mariotel_sessions_cache"][$_GET["id"]]["link"]; ?></p>
                    </div>
                </div>
            </div>
        </div>
    </div>
    <!-- -->
    <div class="wrapper">
        <div class="container-fluid">
            <div class="row">
                <div class="col-md-12">
                    <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post">
                        <div class="alert alert-danger fade in">
                            <input type="hidden" name="id" value="<?php echo trim($_GET["id"]); ?>"/>
                            <p>Confirmez-vous la suppression de cette séance ?</p><br>
                            </b>
                            <p>
                                <input type="submit" value="Oui" class="btn btn-danger">
                                <a href="planning.php" class="btn btn-default">Non</a>
                            </p>
                        </div>
                    </form>
                </div>
            </div>
        </div>
    </div>
</body>
</html>
