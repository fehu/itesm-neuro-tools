Weka ANN Executor
=================

**Requires** `WEKA_HOME` environmental variable set.

Usage:
```bash
sbt "run data_file.arff validationFolds=4 --weka-opts -L 0.1 -N 1000"

# same as

sbt "run data_file.arff validationFolds=4 `cat scripts/config1`"
```

See [scripts/config1](scripts/config1).
