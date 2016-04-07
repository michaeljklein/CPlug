COL=$1
shift;
awk -v col="$COL" '{print $col}' | sed s/\://g
