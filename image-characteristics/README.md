Image Characteristics Extractor
===============================

Extracts characteristic vectors from images regions and writes them in _arff_ format.

Usages:
  1. ```paths to images ... , relation name, target arff file```
  2. ```--dir, path to images directory, relation name, target arff file```

For example:
```bash
img-chv_descriptive-stats_all --dir ~/Pictures/wildfire "wildfire classification" wildfire.arff
```


Requires [java-bridge fork](https://github.com/fehu/haskell-java-bridge-fork)