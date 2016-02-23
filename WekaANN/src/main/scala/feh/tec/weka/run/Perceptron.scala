package feh.tec.weka.run

import java.text.DateFormat
import java.util.{UUID, Date}

import feh.tec.weka.{EvaluationResult, PrepareInstances, MultilayerPerceptronCrossValidation}
import feh.util._
import feh.util.file._
import weka.classifiers.functions.MultilayerPerceptron

object Perceptron extends App{

  val time = new Date

  type ArgGroups = (List[String], List[String], List[(String, String)])

  val (singleArgs, neuroOpts, setArgs_) = Y[(List[String], ArgGroups), ArgGroups](
    rec => {
      case (setter::tail, (acc0, Nil, acc2)) if setter.count('=' ==) == 1 =>
        val Array(left, right) = setter.split('=')
        rec(tail, (acc0, Nil, (left, right) :: acc2))
      case (mod::tail, (acc0, Nil, acc2)) if mod.trim == "--weka-opts" =>
        (acc0, tail , acc2)
      case (arg::tail, (acc0, Nil, Nil)) =>
        rec(tail, (arg::acc0, Nil, Nil))
      case (Nil, (acc0, acc1, acc2)) => (acc0, acc1, acc2)
      case _ => sys.error("wrong arguments")
    }
  )(args.toList -> (Nil, Nil, Nil))

  def group[A, B](x: Traversable[(A, B)]) = x.groupBy(_._1).mapValues(_.map(_._2))

  val setArgs = group(setArgs_).mapValues(_.ensuring(_.size == 1).head)

//  println("args = " + args.toList)

//  println("singleArgs = " + singleArgs)
//  println("neuroOpts =  " + neuroOpts)
//  println("setArgs =  " + setArgs)


  val dataFile = singleArgs.head.toFile

  val reportsDir = setArgs.getOrElse("reports-dir", "reports").toFile                                                     // ARG

  val classAttrName = setArgs.get("class")
  
  val saveModel = setArgs.get("saveModel")


  if (!reportsDir.exists()) reportsDir.mkdir().ensuring(identity[Boolean] _, "failed to create reports directory")

  val validationFolds = setArgs.get("validationFolds").map(_.toInt).getOrElse(10)                                         // ARG

  def newPerceptron = new MultilayerPerceptron $${
    p =>
      p.setOptions(neuroOpts.toArray)                                                                                     // ARG
  }


  val p = new Perceptron

  p.run(dataFile)


  class Perceptron extends MultilayerPerceptronCrossValidation(newPerceptron, validationFolds){
    def prepareInstances =
      is => {
        val clazz = classAttrName map {
          name => is.attribute(name)
        } getOrElse is.attribute(is.numAttributes-1)

        is.setClass(clazz)

        PrepareInstances.dropClasses(Set("Ignore"))(is)
      }

    def extractEvaluationResult = {
      val filename = DateFormat.getDateTimeInstance.format(time) + UUID.randomUUID()
      val file = (reportsDir.getPath / filename).file

      def write = {
        str: String =>
          EvaluationResult.writeToFile(file)(str)
          file.withOutputStream(File.write.utf8("\n\n-- command args: " + args.mkString(" ")), append = true)
          println("wrote report to " + file.getPath)
//          val latest = (reportsDir.getPath / "latest").file
//          val s = s"""ln -s "${file.getPath}" "${latest.getPath}""""
//          println(s)
//          sys.runtime.exec(s).waitFor()
      }

      (c,e) =>
        saveModel.foreach(fname => EvaluationResult.saveModel(fname.toFile)(c,e))
        write(EvaluationResult.fullSummary(c,e))
    }
  }

}

