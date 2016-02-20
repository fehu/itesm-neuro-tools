import java.io.File

val bestToShow = 20

/* Correctly Classified Instances */
val criteriaLinePrefix = "Correctly Classified Instances"
def extractCriteria: String => Double = _.trim.dropWhile(_.isDigit).trim.dropRight(2).toDouble


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

def selectCriteria: Stream[String] => Double = {
  case line #:: tail if line startsWith criteriaLinePrefix =>
    val rest = line.drop(criteriaLinePrefix.size)
    extractCriteria(rest)
  case line #:: tail => selectCriteria(tail)
  case _ => sys.error("criteria line not found")
}

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */



val dir = args(0)
val showAll = args contains "--show-all"
val worst = args contains "--worst"


val files = new File(dir).listFiles().toStream.filter(_.isFile)

val cs = for {
  file <- files
  lines = io.Source.fromFile(file).getLines.toStream
  if lines.headOption map (_.trim) contains "=== Summary ==="
} yield selectCriteria(lines) -> file

val best = {
  val t = cs.sortBy(_._1)
  if (worst) t else t.reverse
}

val toShow = if(showAll) best else best.take(bestToShow)

toShow.foreach{
  case (v, f) => println(s"$v\t${f.getPath}")
}