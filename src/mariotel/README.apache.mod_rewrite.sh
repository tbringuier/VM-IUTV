# Source: https://hostadvice.com/how-to/how-to-enable-apache-mod_rewrite-on-an-ubuntu-18-04-vps-or-dedicated-server/

# ===> Config n°1 <===

# ssh mariotel-debian-root-by-public-IP
a2enmod rewrite
systemctl restart apache2

# Pour vérifier:
# http://tel.marionnet.org/phpinfo.php

# ===> Config n°2 <===

# Ajouter les lignes suivantes dans
# /etc/apache2/sites-available/000-default.conf :
# --- DEBUT
#
#         # JV: pour activer .htaccess et les règles de rewrite:
#         # (plus vraiment nécessaire):
#         <Directory /var/www/html>
#             Options Indexes FollowSymLinks MultiViews
#             AllowOverride All
#             Require all granted
#         </Directory>
#
#         # JV: rediriger tout HTTP -> HTTPS si la requête est sur le port 80
#         # (donc laisser noVNC tranquille en HTTP sur des ports tels que 56900, 57000, etc):
#         # ---
#         # This will enable the Rewrite capabilities:
#         RewriteEngine On
#         # ---
#         # This checks to make sure the connection is not already HTTPS:
#         RewriteCond %{HTTPS} off
#         # ---
#         # This checks to make sure the connection is on port 80:
#         RewriteCond %{SERVER_PORT} 80
#         # ---
#         RewriteRule (.*) https://%{HTTP_HOST}%{REQUEST_URI} [R=301,L]
#         # ---
#
# --- FIN
# </VirtualHost>

systemctl restart apache2

# ---
# JV: Ce qui suit n'est plus utilisé. Je laisse comme doc au cas où ça revienne d'actualité.
# ---

# ===> Config n°3 <===
# Ajouter les lignes de rewrite dans
# /var/www/html/.htaccess :
# --- DEBUT
# RewriteEngine on
# RewriteRule ^addr/([0-9]*)/$ http://tel.marionnet.org:$1/vnc.html [R=301]
# RewriteRule ^slot/([0-9]*)/$ http://tel.marionnet.org:$1/vnc.html [R=301]
# --- FIN

# (Pas besoin de relancer le serveur après modification de .htaccess).

# Tentative à faire (suggestion de Jaime):
# ---
# <VirtualHost *:80>
#         ServerName alligator.lipn.univ-paris13.fr
#         ProxyPreserveHost On
#         ProxyPass / http://localhost:9000/
#         ProxyPassReverse / http://localhost:9000/
# </VirtualHost>
#
# <VirtualHost *:80>
#         ServerName cosyverif.lipn.univ-paris13.fr
#         ProxyPreserveHost On
#         ProxyPass / http://localhost:3000/
#         ProxyPassReverse / http://localhost:3000/
# </VirtualHost>
# ---
# dans /etc/apache2/sites-available/000-default.conf !!!
