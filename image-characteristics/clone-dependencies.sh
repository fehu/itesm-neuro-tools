# Clone all dependencies using HTTPS.
#  Args: 
#        1. target directory
#        2. [ssh] clone using SSH (optional).

WHERE="$1"

PREF_HTTPS="https://github.com/"
PREF_SSH="git@github.com:"

if [ "$2" == "ssh" ]
  then PREF=$PREF_SSH
  else PREF=$PREF_HTTPS
fi

git clone "${PREF}fehu/min-dat--weka-data.git"          "$WHERE/WekaData"
git clone "${PREF}fehu/EitherProjections.git"           "$WHERE/EitherProjections"
git clone "${PREF}fehu/HNat.git"                        "$WHERE/Nat"
git clone "${PREF}fehu/CommandArgs.git"                 "$WHERE/CommandArgs"
git clone "${PREF}fehu/haskell-java-bridge-fork.git"    "$WHERE/java-bridge"
