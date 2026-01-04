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

if(!isset($_SESSION["admin_level"]) || (!($_SESSION["admin_level"] > 0))){
    header("location: booking.php");
    exit;
}

// Include config file
require_once "config.php";

// Define variables and initialize with empty values
$username = $password = $confirm_password = "";
$username_err = $password_err = $confirm_password_err = "";
$checked = "";

// Processing form data when form is submitted
if($_SERVER["REQUEST_METHOD"] == "POST"){

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

    // ---
    $checked = (empty($_POST["admin_level"])) ? "" : "checked";

    // Validate username
    if(empty(trim($_POST["username"]))){
        $username_err = "Please enter a username.";
    } else{
        // Prepare a select statement
        $sql = "SELECT id FROM users WHERE username = ?";

        if($stmt = mysqli_prepare($link, $sql)){
            // Bind variables to the prepared statement as parameters
            mysqli_stmt_bind_param($stmt, "s", $param_username);

            // Set parameters
            $param_username = trim($_POST["username"]);

            // Attempt to execute the prepared statement
            if(mysqli_stmt_execute($stmt)){
                /* store result */
                mysqli_stmt_store_result($stmt);

                if(mysqli_stmt_num_rows($stmt) == 1){
                    $username_err = "This username is already taken.";
                } else{
                    $username = trim($_POST["username"]);
                    $email    = trim($_POST["email"]);

                }
            } else{
                echo "Oops! Something went wrong (1). Please try again later.";
            }

            // Close statement
            mysqli_stmt_close($stmt);
        }
    }

    // Validate password
    if(empty(trim($_POST["password"]))){
        $password_err = "Please enter a password.";
    } elseif(strlen(trim($_POST["password"])) < 6){
        $password_err = "Password must have atleast 6 characters.";
    } else{
        $password = trim($_POST["password"]);
    }

    // Validate confirm password
    if(empty(trim($_POST["confirm_password"]))){
        $confirm_password_err = "Please confirm password.";
    } else{
        $confirm_password = trim($_POST["confirm_password"]);
        if(empty($password_err) && ($password != $confirm_password)){
            $confirm_password_err = "Password did not match.";
        }
    }

    // Check input errors before inserting in database
    if(empty($username_err) && empty($password_err) && empty($confirm_password_err)){

        // Prepare an insert statement
        $sql = "INSERT INTO users (username, password, email, created_by, admin_level) VALUES (?, ?, ?, ?, ?)";

        if($stmt = mysqli_prepare($link, $sql)){
            // Bind variables to the prepared statement as parameters
            mysqli_stmt_bind_param($stmt, "ssssi", $param_username, $param_password, $param_email, $param_created_by, $param_admin_level);

            // Set parameters
            $param_username = $username;
            $param_email    = $email;
            $param_created_by = $_SESSION["username"];
            $param_password = password_hash($password, PASSWORD_DEFAULT); // Creates a password hash
            $param_admin_level = (empty($_POST["admin_level"])) ? 0 : 1;

            // Attempt to execute the prepared statement
            if(mysqli_stmt_execute($stmt)){
                // Redirect to the "welcome" page:
                header("location: success.php");
            } else{
                echo "Something went wrong (2). Please try again later.";
            }

            // Close statement
            mysqli_stmt_close($stmt);
        }
    }

    // Close connection
    mysqli_close($link);
}
?>

<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Inscription</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        body{ font: 16px sans-serif; }
        .wrapper{ width: 350px; padding: 20px; }
    </style>
    <script>
      function show_password() {
        var x = document.getElementById("password1");
        var y = document.getElementById("password2");
        if (x.type === "password") { x.type = "text"; } else { x.type = "password"; }
        if (y.type === "password") { y.type = "text"; } else { y.type = "password"; }
      }
   </script>
</head>
<body>
    <div class="wrapper">
        <h2>Réservation de salles Mariotel</h2>
        <h3><b>Inscription d'un nouvel utilisateur</b></h3>
        <p>Cher utilisateur <b><?php echo $_SESSION["username"]; ?></b>, vous avez le droit d'inscrire un nouvel utilisateur.</p>
        <p>Le nouvel utilisateur pourra effectuer des réservations et, si vous le souhaitez, inscrire à son tour d'autres utilisateurs.</p>
        <p>Veuillez saisir ses futurs identifiants.</p>
        </br>
        <form action="<?php echo htmlspecialchars($_SERVER["PHP_SELF"]); ?>" method="post">
            <!-- -->
            <div class="form-group <?php echo (!empty($username_err)) ? 'has-error' : ''; ?>">
                <label>Utilisateur</label>
                <input type="text" name="username" class="form-control" value="<?php echo $username; ?>">
                <span class="help-block"><?php echo $username_err; ?></span>
            </div>
            <!-- -->
            <div class="form-group <?php echo (!empty($password_err)) ? 'has-error' : ''; ?>">
                <label>Mot de passe</label>
                <input type="password" name="password" id="password1" class="form-control" value="<?php echo $password; ?>">
                <span class="help-block"><?php echo $password_err; ?></span>
            </div>
            <!-- -->
            <div class="form-group <?php echo (!empty($confirm_password_err)) ? 'has-error' : ''; ?>">
                <label>Confirmation du mot de passe</label>
                <input type="password" name="confirm_password" id="password2" class="form-control" value="<?php echo $confirm_password; ?>">
                <span class="help-block"><?php echo $confirm_password_err; ?></span>
            </div>
            <!-- -->
            <input type="checkbox" onclick="show_password()"> Montrer les MDP
            </br>
            </br>
            <!-- -->
            <div class="form-group">
                <label>Email de notification</label>
                <input type="text" name="email" class="form-control" value="">
            </div>
            <!-- -->
            <div class="form-check">
                <label class="form-check-label" for="admin_level">Aura-t-il le droit d'inscrire à son tour d'autres utilisateurs ? </label>
                <input type="checkbox" class="form-check-input" id="admin_level" name="admin_level" <?php echo $checked; ?>>
            </div>
            <!-- -->
            </br>
            <div class="form-group">
                <input type="submit" class="btn btn-primary" value="Inscrire">
                <input type="reset" class="btn btn-default"  value="Effacer">
                <a class="btn btn-link" href="booking.php">Annuler</a>
            </div>
            <!-- -->
        </form>
    </div>
</body>
</html>
