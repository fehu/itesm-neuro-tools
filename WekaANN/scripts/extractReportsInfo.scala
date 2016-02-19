import java.io.{FileWriter, BufferedWriter, File}

import scala.annotation.tailrec


val dir    = args(0)
val target = args(1)

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

type Extracted = Map[String, Either[Double, String]]

case class Extractor(linePrefix: String,
                     extract: String => Extracted)
{
  @tailrec
  final def process(lines: Seq[String]): Extracted = lines match {
    case Seq(line, _*) if line startsWith linePrefix => extract(line.drop(linePrefix.length).trim)
    case Seq(l, tail@_*) => process(tail)
    case Seq() => sys.error("failed to extract " + linePrefix)
  }
}


def CorrectlyClassifiedInstancesS = "Correctly Classified Instances"

lazy val CorrectlyClassifiedInstances = Extractor(
  CorrectlyClassifiedInstancesS,
  s => Map(
    CorrectlyClassifiedInstancesS -> Left(s.dropWhile(_.isDigit).trim.dropRight(2).toDouble)
  )
)

lazy val CommandArgs = {
  def mkR(key: String) = (s"-$key" + """\s+([\d\.]+)""").r
  def sel(key: String): String => Double = mkR(key).findFirstMatchIn _ andThen {
    case Some(m) => m.group(1).toDouble
  }
  def rH = """-H\s+([aiot\d,]+)""".r
  def selH: String => String = rH.findFirstMatchIn _ andThen (_.get.group(1))

  Extractor(
    "-- command args: ",
    s => Map(
      "# of epochs"   -> sel("N"),
      "learning rate" -> sel("L"),
      "momentum"      -> sel("M")
    ).mapValues(Left apply _.apply(s))
    +
      ("hidden layers" -> Right(selH(s)))
  )
}

def simpleExtractor(prefix: String) = Extractor(
  prefix,
  s => Map(prefix -> Left(s.filterNot(_ == '%').toDouble))
)


lazy val extractors = List(
  CorrectlyClassifiedInstances,
  simpleExtractor("Mean absolute error"),
  simpleExtractor("Root mean squared error"),
  simpleExtractor("Relative absolute error"),
  simpleExtractor("Root relative squared error"),
  CommandArgs
)

def extractInf(lines: Seq[String]): Extracted = extractors.flatMap(_.process(lines)).toMap


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


type ExtractedOrd = Ordering[Extracted]


def extractedOrdKey = CorrectlyClassifiedInstances.linePrefix

implicit lazy val ExtractedOrd: ExtractedOrd = Ordering.by((e: Extracted) => e(extractedOrdKey).left.get)


/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


val files = new File(dir).listFiles().toStream.filter(_.isFile)



val extracted = {
  for {
    file <- files
    lines = io.Source.fromFile(file).getLines().toStream
    if lines.headOption map (_.trim) contains "=== Summary ==="
  } yield try extractInf(lines) -> file
          catch { case th: Throwable => println("[Error] failed report: " + file.getPath); throw th }
}.sortBy(_._1)



/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */

//implicit class QuoteString(str: String){
//  def quote: String = "\"" + str + "\""
//}

def toSCSV(e: Extracted, f: File) = {
  val s1 = e.toSeq
    .map(
      _._2.left.map(_.toString)
          .merge
    )
  val s2 = f.getPath

  (s1 :+ s2).mkString(";")
}

/*  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  *  */


val writer = new BufferedWriter(new FileWriter(target))

try{
  val header = extracted.head._1.toSeq.map(_._1).mkString(";") + ";filename"
  writer.append(header).append("\n\n")

  for (e <- extracted) writer.append((toSCSV _).tupled(e)).append('\n')

  writer.flush()
  println("wrote " + target)
}
finally writer.close()










