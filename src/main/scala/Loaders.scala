import java.io.File

import com.github.tototoshi.csv.{CSVReader, DefaultCSVFormat}
import org.saddle.io.CsvParser

import scala.util.Random

abstract class OutlierDataLoader {
  def format(f: Stream[List[String]]): Stream[(List[String],String)] = f.map(r => (r.init, r.last))

  def randomMerge[T](as: Stream[T], bs: Stream[T], ratio: Double): Stream[T] = {
    (as, bs) match {
      case (Stream.Empty, bss) => bss
      case (ass, Stream.Empty) => ass
      case (a #:: ass, b #:: bss) =>
        val dice: Double = Random.nextDouble()
        if (dice >= ratio) a #:: randomMerge(ass, bs, ratio)
        else b #:: randomMerge(as, bss, ratio)
    }
  }

  def loadData(params: Map[String,Any]): (Stream[(List[String],String)], Stream[(List[String],String)])
}

abstract class ColCompareDataLoader {
  def loadData(params: Map[String,Any]): (List[Stream[String]],Map[String,Any])
  def loadGroundTruth(params: Map[String,Any]): (Set[(Int,Int)], Set[(Int, Int)])

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

  override def loadData(params: Map[String, Any]): (Stream[(List[String],String)], Stream[(List[String],String)]) = {
    val errorType: String = params.getOrElse("errorType", "apache2").asInstanceOf[String]
    val allData: Stream[List[String]] = CSVReader.open(kddPath + s"kdd-$errorType.csv").toStream
    val errorString: String = s"$errorType."
    val badPackets: Stream[List[String]] = allData.filter(_.last.contentEquals(errorString))
    val goodPackets: Stream[List[String]] = allData.filter(_.last.contentEquals("normal."))
    val trainAmount: Int = params("trainNum").asInstanceOf[Int]
    val testAmount: Int = params("testNum").asInstanceOf[Int]
    val ratio: Double = params("outlierProp").asInstanceOf[Double]

    val training:Stream[(List[String],String)] = format(goodPackets.take(trainAmount))
    val testing:Stream[(List[String],String)] = format(randomMerge(
      goodPackets.slice(trainAmount,trainAmount+testAmount),
      badPackets, ratio)
    )
    (training, testing)
  }
}

object ForestCoverLoader extends OutlierDataLoader {
  val path: String = s"/Users/ailyas/Documents/Datasets/TwoClassUCI/Forest/"

  override def loadData(params: Map[String,Any]): (Stream[(List[String], String)], Stream[(List[String], String)]) = {
    val outliers: Stream[List[String]] = CSVReader
      .open(path + s"forest_outliers.csv").toStream
      .map(_ :+ "forest_outlier.")
    val inliers: Stream[List[String]] = CSVReader
      .open(path + s"forest_inliers.csv").toStream
      .map(_ :+ "inlier.")

    val outFileSize: Double = new File(path + s"forest_outliers.csv").length()
    val inFileSize: Double = new File(path + s"forest_inliers.csv").length()
    val ratio: Double = outFileSize/(outFileSize+inFileSize)

    val allData: Stream[List[String]] = randomMerge(outliers, inliers, ratio)
    val trainNum: Int = params("trainNum").asInstanceOf[Int]
    val testNum: Int = params("testNum").asInstanceOf[Int]
    val training: Stream[(List[String],String)] = format(allData.take(trainNum))
    val testing: Stream[(List[String],String)] = format(allData.slice(trainNum,trainNum+testNum))
    (training, testing)
  }
}

object MassDataLoader extends OutlierDataLoader {
  val path: String = s"/Users/ailyas/Documents/Datasets/ManualOutlier/"
  val cols: List[String] = (1 to 10).map(_.toString).toList
  val colPaths: List[String] = cols.map(path + "col" + _ + "_done.csv")

  override def loadData(params: Map[String, Any]): (Stream[(List[String], String)], Stream[(List[String], String)]) = {
    val colToLoad: Int = params("col").asInstanceOf[Int]
    val data: Stream[(List[String], String)] = CSVReader
      .open(colPaths(colToLoad)).toStream.map(
      (row: List[String]) => (List(row.head), if(row.last.length==0) "inlier" else "labeled_outlier.")
    )
    (data, data)
  }
}

object MicrobenchLoader {
  val dataPath: String = s"/Users/ailyas/Documents/Datasets/MicrobenchData/"

  def loadSpeedData(len: Int): Stream[String] = CSVReader
    .open(dataPath + "Speed/currency_codes.csv")
    .toStream.map(_.apply(1))

  def loadHingeData(numDates: Int): Stream[String] = CSVReader
    .open(dataPath + "Hinges/" + numDates.toString + "-dates.csv")
    .toStream.map(_.mkString("-"))

  def loadAvgRowLengthData(avgLen: Int): Stream[String] = (1 to 1000).toStream.map(_ => {
    (1 to avgLen).map(_ => Random.nextPrintableChar()).mkString("")
  })

  def loadComparisonData(numCols: Int): List[Stream[String]] = (1 to numCols).map(
    i => CSVReader.open(dataPath + "Comparison/MOCK_DATA.csv")
      .toStream.map(row => row(i % row.length))
  ).toList
}

object FakeDataLoader extends ColCompareDataLoader {

  override def loadData(params: Map[String,Any]): (List[Stream[String]],Map[String,Any]) = {
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
    val res: List[Stream[String]] = (0 until numCols).toList.map(col => {
      (1 to len).toStream.map(_ => generatorsToUse(groupMap(col))())
    })
    (res, params + ("gt" -> gt))
  }

  override def loadGroundTruth(params: Map[String, Any]): (Set[(Int,Int)],Set[(Int,Int)]) = {
    val res: Set[(Int, Int)] = params("gt").asInstanceOf[Set[(Int,Int)]]
    (res, res)
  }
}


object ChemblDataLoader extends ColCompareDataLoader {
  object SemicolonFormat extends DefaultCSVFormat {
    override val delimiter = ';'
  }

  override def loadData(params: Map[String, Any]): (List[Stream[String]],Map[String,Any]) = {
    val allFiles: Seq[File] = new File("/Users/ailyas/Documents/Datasets/SimPairs/chembl/").listFiles()
    val filesToUse: Seq[String] = params.getOrElse("cols", allFiles.map(_.getName)).asInstanceOf[Seq[String]]
    val paths: Seq[String] = allFiles.filter(f => filesToUse.contains(f.getName)).map(_.getAbsolutePath)
    val allCols: List[(String, Seq[Stream[String]])] = paths.toList.map(p => (p.split("/").last, {
      val csvStream: Stream[List[String]] = CSVReader.open(p)(SemicolonFormat).toStream
      csvStream.head.indices map {
        i => csvStream.map(_.apply(i))
      }
    }))
    val _columnNames: List[String] = allCols flatMap { x => x._2.map(x._1 + "|" + _.head) }
    val _columns: List[Stream[String]] = allCols flatMap { _._2 } map { _.tail }

    val goodIndices: Set[Int] = _columns.indices.filter(i => _columns(i).take(100).count(_.length>0) > 50).toSet
    val columnNames: List[String] = _columnNames.zipWithIndex.filter(x => goodIndices.contains(x._2)).map(_._1)
    val columns: List[Stream[String]] = _columns.zipWithIndex.filter(x => goodIndices.contains(x._2)).map(_._1)

    (columns, params + ("colNames" -> columnNames))
  }

  override def loadGroundTruth(params: Map[String, Any]): (Set[(Int,Int)], Set[(Int, Int)]) = {
    val columnNames: List[String] = params("colNames").asInstanceOf[List[String]]
    val threshold: Double = params.getOrElse("threshold",0.5).asInstanceOf[Double]
    val gtCsv: Stream[List[String]] = CSVReader
      .open("/Users/ailyas/Documents/Datasets/SimPairs/manual_labeled_chembl.csv")(SemicolonFormat)
      .toStream
    val rowToTuple: List[String] => (Int, Int) = (row: List[String]) => {
      val firstIn: Int = columnNames.indexOf(row(1).split("/").last + "|" + row(3))
      val secondIn: Int = columnNames.indexOf(row(2).split("/").last + "|" + row(4))
      (firstIn, secondIn)
    }
    val positive: Set[(Int, Int)] = gtCsv.filter(_.last=="1").map(rowToTuple).filter(r => r._1 > -1 && r._2 > -1).toSet
    val negative: Set[(Int, Int)] = gtCsv.filter(_.last=="0").map(rowToTuple).filter(r => r._1 > -1 && r._2 > -1).toSet
    (positive, negative)
  }
}