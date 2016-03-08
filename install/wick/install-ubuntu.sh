!#/bin/bash

################################################################################

echo "Installing GHC and cabal ..."

sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-1.22 ghc-7.10.3
cat >> ~/.bashrc <<EOF
export PATH="\$HOME/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:\$PATH"
EOF
export PATH=~/.cabal/bin:/opt/cabal/1.22/bin:/opt/ghc/7.10.3/bin:$PATH

cabal update

################################################################################

echo "===============================================" 
echo "==========  Installing Oracle JDK 8  =========="
echo "===============================================" 

sudo apt-get install -y python-software-properties
sudo add-apt-repository ppa:webupd8team/java
sudo apt-get update

sudo apt-get install -y oracle-java8-installer

################################################################################

echo "===============================================" 
echo "=====  Installing Dependencies: Libraries  ===="
echo "===============================================" 

sudo apt-get install -y git zlib1g-dev libglib2.0 libcairo2-dev libpango1.0-dev\
                        libgtk-3-dev


################################################################################

echo "===============================================" 
echo "=============  Cloning  Projects  ============="
echo "===============================================" 

git clone https://github.com/fehu/itesm-neuro-tools/

bash itesm-neuro-tools/image-characteristics/clone-dependencies.sh .


################################################################################

echo "===============================================" 
echo "=====  Installing Dependencies: Projects  ====="
echo "===============================================" 

cd Nat
cabal install

cd ../EitherProjections
cabal install

cd ../CommandArgs
cabal install

cd ../WekaData
cabal install

cd ../java-bridge
cabal install


################################################################################

echo "===============================================" 
echo "=========  Generating JNI  Bindings  =========="
echo "===============================================" 


cd ../itesm-neuro-tools/image-characteristics

bash makeWekaInterface.sh

################################################################################

echo "===============================================" 
echo "====  Installing the Rest of Dependencies  ===="
echo "===============================================" 

cabal install alex happy gtk2hs-buildtools

cabal install --dependencies-only


################################################################################

echo "===============================================" 
echo "==============  Installing WICK  =============="
echo "===============================================" 


cabal configure
cabal build
cabal copy
cabal register


################################################################################

wick -h
