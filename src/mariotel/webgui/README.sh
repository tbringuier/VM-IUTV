# ============================================
#            Installation notes
# ============================================

# --------------------------
#           yarn
# --------------------------
# Source: https://classic.yarnpkg.com/en/docs/install#debian-stable
# ---
curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | sudo apt-key add -
echo "deb https://dl.yarnpkg.com/debian/ stable main" | sudo tee /etc/apt/sources.list.d/yarn.list
sudo apt update && sudo apt install yarn
# ---
# type yarn
# yarn is /usr/bin/yarn
# ---

# --------------------------
#      datetimepicker
# --------------------------
# Source: http://eonasdan.github.io/bootstrap-datetimepicker/Installing/
# ---

yarn add eonasdan-bootstrap-datetimepicker

function make_head_inclusions {
 local i
 # ---
 for i in jquery.min.js moment-with-locales.min.js bootstrap.min.js 'bootstrap-datetimepicker.*js'; do find node_modules/ -name "$i"; done \
   | while read path; do echo "<script type=\"text/javascript\" src=\"/$path\"></script>"; done
 # ---
 for i in bootstrap.min.css bootstrap-datetimepicker.css; do find node_modules/ -name "$i"; done \
   | while read path; do echo "<link rel=\"stylesheet\" href=\"/$path\" />"; done
 # ---
}
# ---
# make_head_inclusions
# <script type="text/javascript" src="/node_modules/jquery/dist/jquery.min.js"></script>
# <script type="text/javascript" src="/node_modules/moment/min/moment-with-locales.min.js"></script>
# <script type="text/javascript" src="/node_modules/bootstrap/dist/js/bootstrap.min.js"></script>
# <script type="text/javascript" src="/node_modules/eonasdan-bootstrap-datetimepicker/build/js/bootstrap-datetimepicker.min.js"></script>
# <script type="text/javascript" src="/node_modules/eonasdan-bootstrap-datetimepicker/src/js/bootstrap-datetimepicker.js"></script>
# <link rel="stylesheet" href="/node_modules/bootstrap/dist/css/bootstrap.min.css" />
# <link rel="stylesheet" href="/node_modules/eonasdan-bootstrap-datetimepicker/build/css/bootstrap-datetimepicker.css" />

# --------------------------
#      bootstrap
# --------------------------

# Note: do not launch the command:
# ---
#   yarn add bootstrap
# ---
# because it installs a newer incompatible version of bootstrap.
# The version installed with the datetimepicker should be compatible for all current
# sources in the project.
