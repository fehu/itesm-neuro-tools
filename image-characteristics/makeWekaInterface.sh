# compile java sources
sh java/compile.sh

CLASSPATH="$WEKA_HOME/weka.jar":./weka-calls.jar


CLASSES="weka.classifiers.functions.MultilayerPerceptron\
:weka.classifiers.Classifier\
:weka.classifiers.Evaluation\
:weka.core.converters.ConverterUtils.DataSource\
:weka.core.Instances\
:weka.classifiers.meta.FilteredClassifier\
:weka.filters.unsupervised.attribute.NominalToBinary\
:weka.filters.unsupervised.instance.RemoveWithValues\
:weka.filters.MultiFilter"

CLASSES_CUSTOM="weka.WekaCalls"

rm -rf bindings
j2hs -X -c $CLASSPATH -C $CLASSES:$CLASSES_CUSTOM -p weka -t bindings -n "weka-bindings" -y "0.1.0" "$@"



cd bindings/
cabal configure
cabal build
cabal haddock --hyperlink-source &> /dev/null
echo "Documentation created: dist/doc/html/"
cabal copy
cabal register
cd ..
