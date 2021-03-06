# http://stackoverflow.com/a/32849134/5154287

ROOT_FOLDER=$(\git rev-parse --show-toplevel)
CURR_DIR=$(pwd)
if [ "$ROOT_FOLDER" != "$CURR_DIR" ]
then
  echo "Switch to the root of the repo and try again. Should be in $ROOT_FOLDER"
  exit
fi

cd $ROOT_FOLDER
FILENAME=$*
HASHES=$(\git log --oneline --decorate $FILENAME | ./coln.sh 1)
INDEX=1

for HASH in $HASHES
do
  INDEX_OUT=$(printf %03d $INDEX)
  OUT_FILENAME="$FILENAME.$INDEX_OUT.$HASH"
  echo "saving version $INDEX to file $OUT_FILENAME"
  git show $HASH:$FILENAME > $OUT_FILENAME
  let INDEX=INDEX+1
done
