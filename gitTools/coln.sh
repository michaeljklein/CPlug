# http://stackoverflow.com/a/32849134/5154287

COL=$1
shift;
awk -v col="$COL" '{print $col}' | sed s/\://g
