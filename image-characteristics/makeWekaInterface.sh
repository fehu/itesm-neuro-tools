CLASSES="weka.classifiers.functions.MultilayerPerceptron:weka.classifiers.Classifier:weka.classifiers.Evaluation\
         :weka.core.converters.ConverterUtils.DataSource:weka.core.Instances"

rm -rf bindings
j2hs -Xv -c $WEKA_HOME/weka.jar -C $CLASSES -p weka -t bindings -n "weka-bindings" -y "0.1.0"
