package weka;


import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.core.Instances;
import weka.gui.explorer.ClassifierPanel;

import java.util.Random;
import java.io.*;
import java.util.zip.GZIPOutputStream;

public class WekaCalls{

    public static void crossValidateModel(Evaluation eval, Classifier classifier, Instances data,
                                          int numFolds, Random random) throws Exception {
        eval.crossValidateModel(classifier, data, numFolds, random);
    }

    
    public static void saveModel(String targetFilename, 
                                 String name, 
                                 Classifier classifier, 
                                 Instances instances) throws Exception 
    {
      File sFile = new File(targetFilename);
      
      Instances trainHeader = new Instances(instances, 0);
      
      if (!sFile.getName().toLowerCase().endsWith(ClassifierPanel.MODEL_FILE_EXTENSION)) {
        sFile = new File(sFile.getParent(), sFile.getName()
          + ClassifierPanel.MODEL_FILE_EXTENSION);
      }
      try {
        OutputStream os = new FileOutputStream(sFile);
        if (sFile.getName().endsWith(".gz")) {
          os = new GZIPOutputStream(os);
        }
        ObjectOutputStream objectOutputStream = new ObjectOutputStream(os);
        objectOutputStream.writeObject(classifier);
        trainHeader = trainHeader.stringFreeStructure();
        if (trainHeader != null) {
          objectOutputStream.writeObject(trainHeader);
        }
        objectOutputStream.flush();
        objectOutputStream.close();
      } 
      catch (Exception e) {
        System.out.println("[IO Error] Failed to save the model.");
        throw e;
        }
    }
}