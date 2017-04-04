import org.saddle.{Frame, Series}
import org.saddle.io.{CsvFile, CsvParser}
import org.sameersingh.scalaplot.Implicits._

/**
  * Created by ilyas on 2017-02-18.
  */
object Main {

    def main(args: Array[String]): Unit = {
        runExperimentOutliers("snmpguess")
    }

    def runExperimentOutliers(errType: String) : Unit = {
        val (training, testing) = KDDLoader.loadData(errType)
        val x = new XStruct()
        val newX = x.addNewLines(training.map(_._1.mkString(",")).toArray)
        println(newX.toString)
        println("Classifying...")
        val scores = testing.map {
            case (s: List[String], l: String) => (newX.computeOutlierScore(s.mkString(",")), l == s"$errType.")
        }
        println("Analyzing...")
        val PR: Map[Double, Double] = (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap
        output(PNG("graphs/", errType), xyChart(
            PR.keys.toSeq.sorted -> (PR(_)),
            x = Axis("Recall", range = (0.0,1.05)),
            y = Axis("Precision", range = (0.0,1.05))
        ))
    }

    def runExperimentColCompare(): Unit = {

    }

    def runExperimentColumnLabel(): Unit = {

    }

    def calcPR(scores: List[(Double,Boolean)],threshold: Double): (Double, Double) = {
        val tp = scores.filter(_._1 > threshold).count(_._2).toDouble
        val fp = scores.filter(_._1 > threshold).count(!_._2).toDouble
        val fn = scores.filter(_._1 <= threshold).count(_._2).toDouble
        (tp/(tp+fn), tp/(tp+fp))
    }
}
/*
Load in the saved KDD data. The data was prepared in the following way:
> Download the KDDCup train file, save as kdd-train.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.05' | grep normal > kdd-small.csv
> cat kdd-test.csv | perl -n -e 'print if rand() < 0.001' | grep OTHER_ERR_NAME > kdd-small.csv
 */
object KDDLoader {
    val kddPath = s"/Users/ailyas/Documents/Datasets/KDD1999/"

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