#!/bin/sh
mkdir -p bin
cd src
ghc -o ../bin/paste_raw.cgi LightPaste.hs && strip -p --strip-unneeded --remove-section=.comment -o ../bin/paste.cgi ../bin/paste_raw.cgi
#sudo cp paste.cgi /var/www/paste/
cd ..
