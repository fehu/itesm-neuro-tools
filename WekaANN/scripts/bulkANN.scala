import scala.annotation.tailrec

val arff = args(0)

val jarPath = "./target/scala-2.11/WekaANN-assembly-0.1-SNAPSHOT.jar"

// val pbs = for {
//   t <- (500 to 50000 by 500).toStream // epochs (-N)
//   l <- (0.1 to 1     by 0.1).toStream // learning rate (-L)
//   m <- (0.1 to 1     by 0.1).toStream // momentum (-M)
//   h  <- Stream("a", "t", "i") ++ (20 to 100 by 10).toStream.map(_.toString)       // hidden layer (1st layer)
//   h2 <- Stream(null, "a", "t", "i") ++ (20 to 100 by 10).toStream.map(_.toString) // hidden layer (2nd layer)
// }


val pbs = for {
  h2 <- Stream(null, "a", "t") // ++ Stream(10, 20, 40, 100).toStream.map(_.toString) // hidden layer (2nd layer)       -- 3    288
  h  <- Stream("a", "t")       // ++ Stream(10, 20, 40, 100).map(_.toString)       // hidden layer (1st layer)          -- 2    96
  m <-         (0.2  to 1     by 0.3).toStream // momentum (-M)                                                         -- 3    48
  l <- 0.1 #:: (0.2  to 1     by 0.3).toStream // learning rate (-L)                                                    -- 4    16
  t <- 500 #:: (1000 to 3000  by 1000).toStream // epochs (-N)                                                          -- 4
  
}
  yield {
    val cmd = List(
      "java", "-jar", jarPath,
      arff, "validationFolds=10",
      "--weka-opts",
      "-N", t.toString,
      "-L", l.toString,
      "-M", m.toString,
      "-H", h + Option(h2).map("," + _).getOrElse("")
    )
//    val cmd = s"""run $arff validationFolds=10 --weka-opts -N $t -L $l -M $m -H ${h + Option(h2).map(","+).getOrElse("")}"""
    new ProcessBuilder(cmd: _*)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .redirectOutput(ProcessBuilder.Redirect.INHERIT)
  }


def exec(pb: ProcessBuilder) = () => {
  println("Executing: " + pb.command().toArray.mkString(" "))
  pb.start().waitFor()
  ()
}

//pbs.foreach(exec)

val pool = new ThreadPool(8, pbs.map(exec))




class ThreadPool(size: Int, var jobs: Seq[() => Unit]){
  protected val threads = List.tabulate(size){
    i =>
      val t = new Thread(new Executor(i))
      t.start()
      t
  }

  def nextJob(): Option[() => Unit] = synchronized{
    jobs.headOption.map{
      h =>
        jobs = jobs.tail
        h
    }
  }

  class Executor(val id: Int) extends Runnable{
    @tailrec
    final def run() = nextJob() match {
      case Some(job) => job(); run()
      case _         =>
    }
  }
}
