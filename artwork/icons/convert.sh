#!/usr/bin/env sh
for i in *.svg; 
do 
echo $i
node ../../nodes/api/priv/www/js/scripts/svgtoicon.js $i ../../nodes/api/priv/www/images/icons/$i; 
done
