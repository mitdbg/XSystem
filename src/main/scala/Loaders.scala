import java.io.File

import org.saddle.{Frame, Series}
import org.saddle.io.{CsvFile, CsvParams, CsvParser}

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

abstract class ColCompareDataLoader {
  def loadData(params: Map[String,Any]): (List[List[String]],Map[String,Any])
  def loadGroundTruth(params: Map[String,Any]): Set[(Int,Int)]

  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]): Traversable[(X,Y)] = for { x <- xs; y <- ys } yield (x, y)
  }
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

object FakeDataLoader extends ColCompareDataLoader {

  override def loadData(params: Map[String,Any]): (List[List[String]],Map[String,Any]) = {
    val (len: Int, numGroups: Int, numCols: Int) = (
      params.getOrElse("length", 1000).asInstanceOf[Int],
      params.getOrElse("numGroups", 50).asInstanceOf[Int],
      params.getOrElse("numCols", 100).asInstanceOf[Int]
    ) // Getting user-defined options
    val splitters: Seq[Int] = Seq(0) ++
      Random.shuffle((0 until numCols).toList).take(numGroups).sorted ++
      Seq(numCols)// Get random numbers
    val groups: Seq[Seq[Int]] = splitters.indices.tail.map(i => splitters(i-1) until splitters(i))
    val groupMap: Map[Int, Int] = groups.indices.flatMap(
      i => groups(i).map(
        j => j -> i
      )
    ).toMap
    val gt: Set[(Int, Int)] = groups.flatMap(g => g.cross(g).toSeq).toSet
    val generatorsToUse: List[()=>String] = Random.shuffle(Config.acceptableGenerators).take(groups.length)
    val res: List[List[String]] = (1 to len).map(
      _ => (0 until numCols).toList.map(
        i => generatorsToUse(groupMap(i)).apply()
      )
    ).toList
    (res.transpose, params + ("gt" -> gt))
  }

  override def loadGroundTruth(params: Map[String, Any]): Set[(Int,Int)] = {
    params("gt").asInstanceOf[Set[(Int,Int)]]
  }
}

object ChemblDataLoader extends ColCompareDataLoader {
  override def loadData(params: Map[String, Any]): (List[List[String]],Map[String,Any]) = {
    val allFiles: Seq[File] = new File("/Users/ailyas/Documents/Datasets/SimPairs/chembl/").listFiles()
    val filesToUse: Seq[String] = params.getOrElse("cols", allFiles.map(_.getName)).asInstanceOf[Seq[String]]
    val paths: Seq[String] = allFiles.filter(f => filesToUse.contains(f.getName)).map(_.getAbsolutePath)
    val columns: List[List[String]] = paths.toList.flatMap(p => {
      println(p)
      CsvParser.parse(params=CsvParams(separChar = ';'))(CsvFile(p))
        .toColSeq
        .tail
        .map(_._2.toSeq.toList.map(_._2))
    })
    val columnNames: List[String] = paths.toList.flatMap(p => {
      CsvParser.parse(params=CsvParams(separChar = ';'))(CsvFile(p))
        .rowAt(0)
        .toSeq.map(_._2 + "|" + p.split("/").last)
    })
    (columns, params + ("colNames" -> columnNames))
  }

  override def loadGroundTruth(params: Map[String, Any]): Set[(Int,Int)] = {
    val columnNames: List[String] = params("colNames").asInstanceOf[List[String]]
    val threshold: Double = params.getOrElse("threshold",0.5).asInstanceOf[Double]
    val gtCsv = CsvParser.parse(CsvFile("/Users/ailyas/Documents/Datasets/SimPairs/sim_pairs_chembl.csv"))
    val gt: Seq[(Int,Int)] = gtCsv.toRowSeq.map {
      case (i: Int, r: Series[Int, String]) => {
        val indOne: Int = columnNames.indexOf(r.at(1) + "|" + r.at(0))
        val indTwo: Int = columnNames.indexOf(r.at(3) + "|" + r.at(2))
        if (r.last.get.toDouble > threshold && indOne > -1 && indTwo > -1) (indOne, indTwo) else null
      }}.filter(_!=null)
    gt.toSet
  }
}