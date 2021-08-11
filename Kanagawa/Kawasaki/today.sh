#!/bin/bash

LF=$(printf '\\\012_')
LF=${LF%_}

sed -e 's/[0-9]\{1,\}例目/'"$LF"'/g'|\
sed -e 's/代//g'|\
sed -e 's/10歳未満/10/g'|\
sed -e 's/100歳以上/90/g'|\
sed -e 's/川崎市//g'|\
sed -e 's/区//g'|\
sed -e 's/市外（..）/市外/g'|\
#sed -e 's/市外（..）//g'|\
sed -e '/^$/d'|\

awk -F " " -v today="$(date +%F)" '{printf "\"" today "\",\"" $4 "\",\"" $2 "\",\"" $3 "\"\n"}'
