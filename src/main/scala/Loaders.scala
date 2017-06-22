import org.saddle.{Frame, Series}
import org.saddle.io.{CsvFile, CsvParser}
import faker._

import scala.util.Random

abstract class OutlierDataLoader {

  def formatFrame(f: Frame[Int,Int,String]): List[(List[String],String)] = f.colSlice(0,f.numCols-1)
    .rreduce(_.toSeq.map(_._2).toList)
    .toSeq
    .map(_._2)
    .toList
    .zip(
      f.colAt(f.numCols-1)
        .toSeq
        .map(_._2)
    )

  def loadData(params: Map[String,Any]): (List[(List[String],String)], List[(List[String],String)])
}

/*
Load in the saved KDD data. The data was prepared in the following way:
> Download the KDDCup train file, save as kdd-train.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.05' | grep normal > kdd-small.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.001' | grep OTHER_ERR_NAME > kdd-small.csv
 */
object KDDLoader extends OutlierDataLoader {
  val kddPath: String = s"/Users/ailyas/Documents/Datasets/KDD1999/"

  override def loadData(params: Map[String, Any]): (List[(List[String],String)], List[(List[String],String)]) = {
    val errorType: String = params.getOrElse("errorType", "default").asInstanceOf[String]
    val allData: Frame[Int,Int,String] = CsvParser.parse(CsvFile(kddPath + s"kdd-$errorType.csv"))
    val badPackets: Frame[Int,Int,String] = allData.rfilter(
      (x: Series[Int, String]) => x.last.toString.equals(s"$errorType.")
    )
    val goodPackets: Frame[Int,Int,String] = allData.rfilter(
      (x: Series[Int, String]) => x.last.toString.equals("normal.")
    )

    val training:List[(List[String],String)] = formatFrame(goodPackets.rowSlice(0,(goodPackets.numRows*0.9).toInt))
    val testing:List[(List[String],String)] = formatFrame(goodPackets.rowSlice((goodPackets.numRows*0.9).toInt, goodPackets.numRows)
      .rjoin(badPackets))
    (training, testing)
  }
}

object ForestCoverLoader extends OutlierDataLoader {
  val path: String = s"/Users/ailyas/Documents/Datasets/TwoClassUCI/Forest/"

  override def loadData(params: Map[String,Any]): (List[(List[String], String)], List[(List[String], String)]) = {
    val outliers: Frame[Int,Int,String] = CsvParser.parse(CsvFile(path + s"forest_outliers.csv"))
    val inliers: Frame[Int,Int,String] = CsvParser.parse(CsvFile(path + s"forest_inliers.csv"))
    val allData = Random.shuffle(formatFrame(outliers.concat(inliers)))
    val trainPct: Double = params.getOrElse("trainPct", 0.9).asInstanceOf[Double]
    val training: List[(List[String],String)] = allData.slice(0,(trainPct*allData.length).toInt)
    val testing: List[(List[String],String)] = allData.slice((trainPct*allData.length).toInt,allData.length)
    (training, testing)
  }
}

object MicrobenchLoader {
  val dataPath: String = s"/Users/ailyas/Documents/Datasets/MicrobenchData/"

  def loadSpeedData(len: Int): List[String] = CsvParser.parse(
    CsvFile(dataPath + "Speed/currency_codes.csv")
  ).colAt(1).toSeq.map(_._2).toList

  def loadHingeData(numDates: Int): List[String] = CsvParser.parse(
    CsvFile(dataPath + "Hinges/" + numDates.toString + "-dates.csv")
  ).rreduce(x => x.toSeq.map(_._2).mkString("-")).toSeq.map(_._2).toList

  def loadAvgRowLengthData(avgLen: Int): List[String] = (1 to 1000).map(_ => {
    (1 to avgLen).map(_ => Random.nextPrintableChar()).mkString("")
  }).toList
}

object FakeDataLoader {
  def loadTable(len: Int): (List[List[String]],List[(Int,Int)]) = ((1 to len).map(_ => {
    List(
      Internet.user_name,
      Internet.user_name,
      Internet.email,
      Internet.email,
      Internet.ip_v4_address,
      Internet.domain_name,
      Internet.ip_v4_address,
      Name.first_name,
      Name.last_name
    )
  }).toList.transpose, List((0,1),(2,3),(4,6),(7,8)))
}