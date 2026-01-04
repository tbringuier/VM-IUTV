# Here, in the repository:
mkdir -p UNISON
cd UNISON

# Source: https://forums.linuxmint.com/viewtopic.php?t=323420
wget http://mirrors.kernel.org/ubuntu/pool/universe/u/unison/unison_2.48.4-1ubuntu1_amd64.deb

# Extract with alien:
sudo apt install -y alien
alien --to-tgz unison_2.48.4-1ubuntu1_amd64.deb
tar -xvf unison-2.48.4.tgz ./usr/bin/unison-2.48.4
mv usr/bin/unison-2.48.4* .

# Mr proper:
rmdir ./usr/bin/
rmdir ./usr/
rm -f unison_2.48.4-1ubuntu1_amd64.deb unison-2.48.4.tgz

# Return to parent:
cd ..

chown -R mariotel:mariotel UNISON


