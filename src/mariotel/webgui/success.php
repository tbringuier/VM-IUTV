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

// Initialize the session
session_start();
// Check if the user is logged in, if not then redirect him to the login page
if(!isset($_SESSION["loggedin"]) || $_SESSION["loggedin"] !== true){
    header("location: login.php");
    exit;
}
?>
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Succès</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .wrapper{ font: 16px sans-serif; width: 600px; margin: 0 auto;}
    </style>
</head>
<body>
    <div class="wrapper">
        <div class="container-fluid">
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>Succès !</h1>
                    </div>
                    <div class="alert alert-success">
                      <strong>Votre requête a été validée</strong>
                    </div>
                    <p>Vous pouvez <a href="javascript:history.go(-2)" class="alert-link">reprendre</a> une activité normale (cit.)</p>
                </div>
            </div>
        </div>
    </div>
</body>
</html>
