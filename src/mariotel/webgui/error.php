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
?>
<!DOCTYPE html>
<html lang="fr">
<head>
    <meta charset="UTF-8">
    <title>Error</title>
    <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
    <style type="text/css">
        .wrapper{
            width: 750px;
            margin: 0 auto;
        }
    </style>
</head>
<body>
    <div class="wrapper">
        <div class="container-fluid">
            <div class="row">
                <div class="col-md-12">
                    <div class="page-header">
                        <h1>Requête invalide</h1>
                    </div>
                    <div class="alert alert-danger fade in">
                        <p>Désolé, vous avez produit une requête invalide.
                           Vous pouvez <a href="javascript:history.go(-1)" class="alert-link">essayer</a> à nouveau.</p>
                    </div>
                </div>
            </div>
        </div>
    </div>
</body>
</html>
