#!/usr/bin/env sh

# region borken
types="area countries climate continent geojson positions provinces superregion winds"

for t in $types
do
  ghc -Wall -O2 -funbox-strict-fields generate-$t.hs -o generate-$t
done

for t in $types
do
  (printf "var $t = "; "./generate-$t") > "$t.js" &
done

wait
