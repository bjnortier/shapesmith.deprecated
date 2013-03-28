#!/usr/bin/env sh
for i in *.svg; 
do 
echo $i
node ../../js/scripts/svgtoicon.js $i ../../images/icons/$i; 
done
