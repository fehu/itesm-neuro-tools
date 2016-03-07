Image Characteristics Extractor
===============================

Requires:

* [java-bridge fork](https://github.com/fehu/haskell-java-bridge-fork)
* [CommandArgs] (https://github.com/fehu/CommandArgs)
* [Nat] (https://github.com/fehu/HNat)
* [Weka Data] (https://github.com/fehu/min-dat--weka-data)


### Usage

```
	Wildfire Image Classification Kit (WICK)
	---------------------------------  ----

Conatains tools for 1) image characteristics extraction
                       (descriptive statistics);
                    2) Weka's Multilayer Perceptron model training;
                    3) image classification, using the model.

wick <mode> <source> <target> [help] [verbosity] [save-regions] [name] [no-class] [validate] [x-report] [options] [tikz-confusion] [gui] [par]

Positional:
  mode ::
    1. arff  -- extract characteristic vectors from images in given directory;
    2. model -- train perceptron model, given the characteristic vectors;
    3. class -- classify images in given directory using given model.
  source :: Text
    Depending on the <mode>:
    	1. arff  -- a directory with images to process;
    	2. model -- *.arff data file with classes assigned;
    	3. class -- model file to be used for classification.
  target :: Text
    Depending on the <mode>:
    	1. arff  -- destination *.arff file;
    	2. model -- destination *.model file;
    	3. class -- images directory.

Optional:

  help <cmd...>
     -h --help
     Show help
        cmd... :: Text 	--  Commands to show the help for

  verbosity <value>
     -V --verbosity
     Set verbosity
        value :: Verbosity
          verbosity level: 0-3
          or 'silent', 'errors', 'warn', 'full'

  save-regions <value>
     --save-regions
     [ARFF] Save region images, with assigned class.
        value :: Text 	--  root directory

  name <value>
     -n --name
     [ARFF] Relation name.
        value :: Text 	--  Name.

  no-class
     -? --no-class
     [ARFF] Do not interrogate classes and set ?.

  validate <value>
     -x --validate
     [MODEL] Cross validate model.
        value :: Int 	--  validation folds

  x-report <value>
     --x-report
     [MODEL] Save cross validation report.
        value :: Text 	--  report file

  options <value>
     -o --options
     [MODEL] Set Multilayer Perceptron options (see Weka).
        value :: Text 	--  configuration should be put in "" quotes to avoid separation

  tikz-confusion <value>
     --tikz-confusion
     [MODEL] Write confusion diagram Tikz source (LaTeX).
        value :: Text 	--  diagram file

  gui
     -G --gui
     [CLASS] Show classification results in GUI (uses GTK).

  par <value>
     --par
     [CLASS] Run classification in parallel threads (experimental).
        value :: Int 	--  number of threads

```

##### Examples

```
wick arff  ~/Pictures/wildfire fire1.arff --save-regions reports --tikz-confusion confusion.tikz.tex

wick model fire1.arff fire1.model -x 4 --x-report report.log -o "-N 100 -L 0.9 -M 0.9"

wick class fire1.model ~/Pictures/wildfire -G
```

### Classes

The classes used are defined in [WildfireClass.hs](exec/WildfireClass.hs).
At the moment following classes are defined:

```haskell
data WildfireClass = Fire   -- ^ Region contains fire.
                   | Smoke  -- ^ Region contains smoke, but no sign fire.
                   | None   -- ^ Region contains neither.
                   | Ignore -- ^ Do not use the region for training.
                   | Unknown
                   deriving (Enum, Bounded, Eq)
```
Show instance is declared apart in order to provide different names, like _Fire & Smoke_.
