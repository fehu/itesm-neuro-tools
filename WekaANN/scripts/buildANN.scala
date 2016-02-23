import scala.annotation.tailrec

val arff = args(0)

val jarPath = "./target/scala-2.11/WekaANN-assembly-0.1-SNAPSHOT.jar"


val h = "a" // "t,t"
val m = 0.2
val l = 0.2
val t = 500

val cmd = List(
  "java", "-jar", jarPath,
  arff, "validationFolds=10",
  "saveModel=wildfire01.model",
  "--weka-opts",
  "-N", t.toString,
  "-L", l.toString,
  "-M", m.toString,
  "-H", h
)
new ProcessBuilder(cmd: _*)
  .redirectError(ProcessBuilder.Redirect.INHERIT)
  .redirectOutput(ProcessBuilder.Redirect.INHERIT)
  .start()