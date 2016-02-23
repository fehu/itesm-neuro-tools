CLASSES="weka.classifiers.functions.MultilayerPerceptron:weka.classifiers.Classifier:weka.classifiers.Evaluation\
         :weka.core.converters.ConverterUtils.DataSource:weka.core.Instances"

rm -rf bindings
j2hs -X -c $WEKA_HOME/weka.jar -n $CLASSES -t bindings
