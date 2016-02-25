package weka;


import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.core.Instances;
import weka.gui.explorer.ClassifierPanel;
import weka.core.SerializationHelper;
import weka.filters.MultiFilter;
import weka.filters.Filter;

import java.util.Random;
import java.io.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

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
      String fName = targetFilename;
      
      Instances trainHeader = new Instances(instances, 0);
      
      if (!fName.toLowerCase().endsWith(ClassifierPanel.MODEL_FILE_EXTENSION)) {
        fName = fName + ClassifierPanel.MODEL_FILE_EXTENSION;
      }
      SerializationHelper.writeAll(fName, new Object[]{classifier, trainHeader});
      
    }
    
    public static List<Object> newList() { return new ArrayList<Object>(); }
    
    
    public static List<Filter> filtersList(MultiFilter mf) {
      return new ArrayList<Filter>(Arrays.asList(mf.getFilters()));
    }
    
    public static void setFiltersList(MultiFilter mf, List<Filter> fs) {
      mf.setFilters(fs.toArray(mf.getFilters()));
      return;
    }
    
//     public static MultiFilter /*appendFilter(MultiFilter mf, Filter f) {
//       Filter[] fs = mf.getFilters();
//       ArrayList<Filter> al = new ArrayList<Filter>(Arrays.asList(fs));
//       al.add(f);
//       mf.setFilters(al.toArray(fs));
//       return mf;
//     }

}