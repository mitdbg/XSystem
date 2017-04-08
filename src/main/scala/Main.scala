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
        val trainedX = x.addNewLines(training.map(_._1.mkString(",")).toArray)
        println(trainedX.toString)
        println("Classifying...")
        val scores = testing.map {
            case (s: List[String], l: String) => (trainedX.computeOutlierScore(s.mkString(",")), l == s"$errType.")
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
        val ds = DuplicateDetectionLoader.loadData()
        var results: Map[(Int,Int),Double] = Map()
        ds._1.indices.cross(ds._2.indices).foreach {
            case (i: Int, j: Int) => {
                val x = new XStruct()
                val trainedX = x.addNewLines(ds._1(i).toArray)
                val y = new XStruct()
                val trainedY = y.addNewLines(ds._2(j).toArray)
                results += ((i,j)->)
            }
        }
    }

    def runExperimentColumnLabel(): Unit = {

    }

    def calcPR(scores: List[(Double,Boolean)],threshold: Double): (Double, Double) = {
        val tp = scores.filter(_._1 > threshold).count(_._2).toDouble
        val fp = scores.filter(_._1 > threshold).count(!_._2).toDouble
        val fn = scores.filter(_._1 <= threshold).count(_._2).toDouble
        (tp/(tp+fn), tp/(tp+fp))
    }

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]): Traversable[(X,Y)] = for { x <- xs; y <- ys } yield (x, y)
    }
}
