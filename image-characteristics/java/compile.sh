# from http://stackoverflow.com/questions/59895/can-a-bash-script-tell-what-directory-its-stored-in
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

CLASSES_DIR="$DIR/classes/"

rm -rf $CLASSES_DIR
mkdir $CLASSES_DIR

javac -cp $WEKA_HOME/weka.jar -d $CLASSES_DIR $DIR/src/*

# -sourcepath ./src/  