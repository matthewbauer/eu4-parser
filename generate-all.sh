#!/usr/bin/env sh

types="area climate continent geojson provinces region superregion"
for t in $types
do
  (echo -n "var $t = "; runhaskell "generate-$t.hs") > "$t.js"
done
