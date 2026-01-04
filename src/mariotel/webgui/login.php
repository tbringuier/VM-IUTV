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

// ---
// Based on (thanks):
// https://www.tutorialrepublic.com/php-tutorial/php-mysql-login-system.php
// ---

// Debugging:
ini_set('display_errors', 'on');

// Initialize the session
session_start();

// Check if the user is already logged in, if yes then redirect him to welcome page
if(isset($_SESSION["loggedin"]) && $_SESSION["loggedin"] === true){
  header("location: booking.php");
  exit;
}

// Include config file
require_once "config.php";

// Define variables and initialize with empty values
$username = $password = "";
$username_err = $password_err = "";

// Processing form data when form is submitted
if($_SERVER["REQUEST_METHOD"] == "POST"){

    // Check if username is empty
    if(empty(trim($_POST["username"]))){
        $username_err = "Please enter username.";
    } else{
        $username = trim($_POST["username"]);
    }

    // Check if password is empty
    if(empty(trim($_POST["password"]))){
        $password_err = "Please enter your password.";
    } else{
        $password = trim($_POST["password"]);
    }

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

    // Validate credentials
    if(empty($username_err) && empty($password_err)){
        // Prepare a select statement
        $sql = "SELECT id, username, password, email, admin_level FROM users WHERE username = ?";

        if($stmt = mysqli_prepare($link, $sql)){
            // Bind variables to the prepared statement as parameters
            mysqli_stmt_bind_param($stmt, "s", $param_username);

            // Set parameters
            $param_username = $username;

            // Attempt to execute the prepared statement
            if(mysqli_stmt_execute($stmt)){
                // Store result
                mysqli_stmt_store_result($stmt);

                // Check if username exists, if yes then verify password
                if(mysqli_stmt_num_rows($stmt) == 1){
                    // Bind result variables
                    mysqli_stmt_bind_result($stmt, $id, $username, $hashed_password, $email, $admin_level);
                    if(mysqli_stmt_fetch($stmt)){
                        if(password_verify($password, $hashed_password)){
                            // Password is correct, so start a new session
                            session_start();
                            //---
                            // Store data in session variables
                            $_SESSION["loggedin"] = true;
                            $_SESSION["id"] = $id;
                            $_SESSION["username"] = $username;
                            $_SESSION["email"] = $email;
                            $_SESSION["admin_level"] = $admin_level;
                            //---
                            // Redirect user to welcome page
                            header("location: booking.php");
                        } else{
                            // Display an error message if password is not valid
                            $password_err = "Le mot de passe saisi n'est pas valide.";
                        }
                    }
                } else{
                    // Display an error message if username doesn't exist
                    $username_err = "Pas de compte au nom de cet utilisateur.";
                }
            } else{
                echo "Oops! Something went wrong. Please try again later.";
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
    <title>Login</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
       body{ font: 14px sans-serif; }
       .wrapper{ width: 350px; padding: 20px; }
       .glyphicon.glyphicon-info-sign { font-size: 20px; }
    </style>
</head>
<body>
    <div class="wrapper">
        <h2>Réservation de salles Mariotel<?php echo_info_anchor(); ?></h2>
        <h2><b>Connexion</b></h2>
        <p>Veuillez saisir vos identifiants.</p>
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
                <input type="password" name="password" class="form-control">
                <span class="help-block"><?php echo $password_err; ?></span>
            </div>
            <!-- -->
            <div class="form-group">
                <input type="submit" class="btn btn-primary" value="Connexion">
            </div>
            <!-- -->
            <!-- <p>Vous n'avez pas un compte ? <a href="register.php">Enregistrez-vous maintenant</a>.</p> -->
            <!-- -->
        </form>
    </div>
</body>
</html>
