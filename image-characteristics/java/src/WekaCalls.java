package weka;


import weka.classifiers.Classifier;
import weka.classifiers.Evaluation;
import weka.core.Instances;
import weka.core.Instance;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.gui.explorer.ClassifierPanel;
import weka.core.SerializationHelper;
import weka.filters.MultiFilter;
import weka.filters.Filter;

import util.Pair;

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
    
    public static Pair<Classifier, Instances> loadModel(String modelFilename) throws Exception {
      Object o[] = SerializationHelper.readAll(modelFilename);
      return new Pair<Classifier, Instances>((Classifier) o[0], (Instances) o[1]);
    }
    
    
    public static List<Object> newList() { return new ArrayList<Object>(); }
    
    
    public static List<Filter> filtersList(MultiFilter mf) {
      return new ArrayList<Filter>(Arrays.asList(mf.getFilters()));
    }
    
    public static void setFiltersList(MultiFilter mf, List<Filter> fs) {
      mf.setFilters(fs.toArray(mf.getFilters()));
      return;
    }
    
    public static void setOptions(Classifier c, List<String> ops) throws Exception {
      String[] res = {};
      c.setOptions(ops.toArray(res));
      return;
    }
    
    
    public static Attribute newAttrNom(String attributeName, FastVector attributeValues){
      return new Attribute(attributeName, attributeValues);
    }
    
    public static Attribute newAttrNum(String attributeName){
      return new Attribute(attributeName);
    }
    
    public static Instance newInstance(int numAttributes){
      return new Instance(numAttributes);
    }
    
//     public static MultiFilter /*appendFilter(MultiFilter mf, Filter f) {
//       Filter[] fs = mf.getFilters();
//       ArrayList<Filter> al = new ArrayList<Filter>(Arrays.asList(fs));
//       al.add(f);
//       mf.setFilters(al.toArray(fs));
//       return mf;
//     }

}