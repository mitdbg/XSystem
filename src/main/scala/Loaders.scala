import org.saddle.{Frame, Series}
import org.saddle.io.{CsvFile, CsvParser}

import scala.util.Random

/*
Load in the saved KDD data. The data was prepared in the following way:
> Download the KDDCup train file, save as kdd-train.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.05' | grep normal > kdd-small.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.001' | grep OTHER_ERR_NAME > kdd-small.csv
 */
object KDDLoader {
  val kddPath: String = s"/Users/ailyas/Documents/Datasets/KDD1999/"

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

  def loadData(errorType:String): (List[(List[String],String)],List[(List[String],String)]) = {
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

object DuplicateDetectionLoader {
  val dupPath: String = s"/Users/ailyas/Documents/Datasets/DupDetect/"
  val groundTruth: Set[(Int,Int)] = Set(2->0,3->1,4->2,5->3,6->4,7->5)

  def getCols(f: Frame[Int,Int,String]): List[List[String]] = f.toColSeq.map(_._2).map(_.toSeq.map(_._2).toList).toList

  def loadData(): (List[List[String]], List[List[String]]) = {
    val datasetOne: Frame[Int,Int,String] = CsvParser.parse(CsvFile(dupPath + "reg_dataset1.csv"))
    val datasetTwo: Frame[Int,Int,String] = CsvParser.parse(CsvFile(dupPath + "reg_dataset2.csv"))
    (getCols(datasetOne), getCols(datasetTwo))
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