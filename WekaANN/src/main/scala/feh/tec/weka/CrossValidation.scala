package feh.tec.weka

import java.util
import java.util.Random

import feh.util.file._
import weka.classifiers.functions.MultilayerPerceptron
import weka.classifiers.{Classifier, Evaluation}
import weka.core.converters.ConverterUtils.DataSource
import weka.core.{Attribute, Instances}

import scala.collection.convert.decorateAsScala._
import scala.util.{Failure, Success}


abstract class CrossValidation[C <: Classifier] {
  def validationFolds: Int
  def prepareInstances: Instances => Instances
  def extractEvaluationResult: Evaluation => Unit

  protected val classifier: C


  def run(file: File): Unit = {
    val instances = file.withInputStream(DataSource.read) match {
      case Success(insts) => insts
      case Failure(ex)    => throw ex
    }

    val insts = prepareInstances(instances)
    val evaluation = new Evaluation(instances)
    evaluation.crossValidateModel(classifier, insts, validationFolds, new Random)
    extractEvaluationResult(evaluation)
  }
}

abstract class MultilayerPerceptronCrossValidation(protected val classifier: MultilayerPerceptron,
                                                   val validationFolds: Int = 10)
  extends CrossValidation[MultilayerPerceptron]






object PrepareInstances{

  def dropClasses(toDrop: Set[String]): Instances => Instances = {
    is =>
      val drop = toDrop.map(is.classAttribute.indexOfValue)

      println("dropping instances with class in " + toDrop)

      for {
        i <- (0 until is.numInstances()).reverse
        inst = is.instance(i)
        j = inst.value(is.classAttribute).toInt
        if drop contains j
      } is.delete(i)

      is
  }

  def dropAttributes(toDrop: Set[String]): Instances => Instances = {
    is =>
      toDrop.foreach(a => is.deleteAttributeType(is.attribute(a).`type`))
      is
  }

//  def onlyAttributes(attrs: Set[String]): Instances => Instances = {
//    is =>
//      is.enumerateAttributes.asInstanceOf[util.Enumeration[Attribute]].asScala.foreach{
//        a => if (!attrs.contains(a.name)) is.deleteAttributeType(a.`type`)
//      }
//    is
//  }

}




object EvaluationResult{

  def writeToFile(file: File): String => Unit = s => file.withOutputStream(File.write.utf8(s))

  def fullSummary(e: Evaluation) = List(
    summary(e),
    e.toMatrixString,
    e.toClassDetailsString,
    e.toCumulativeMarginDistributionString
  ).mkString("\n"*3)

  def summary(e: Evaluation) = e.toSummaryString(true)

}