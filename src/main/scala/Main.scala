import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.XYData

/**
  * Created by ilyas on 2017-02-18.
  */
object Main {

    def main(args: Array[String]): Unit = {
        //runExperimentOutliers("snmpguess")
        //runExperimentColCompare()
        runExperimentMicrobench(runCL = true, runNH = true, runARL = false)
    }

    def runExperimentOutliers(errType: String) : Unit = {
        val (training, testing) = KDDLoader.loadData(errType)
        val x = new XStruct().addNewLines(training.map(_._1.mkString(",")).toArray)
        println(x.toString)
        println("Classifying...")
        val scores = testing.map {
            case (s: List[String], l: String) => (x.computeOutlierScore(s.mkString(",")), l == s"$errType.")
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
        val learnStructs = (s:List[List[String]]) => s.map(x => new XStruct().addNewLines(x.toArray))
        val (d1: List[List[String]], d2:List[List[String]]) = DuplicateDetectionLoader.loadData()
        val gt: Set[(Int, Int)] = DuplicateDetectionLoader.groundTruth
        val xs: (List[XStruct], List[XStruct]) = (learnStructs(d1), learnStructs(d2))
        var scores: List[(Double, Boolean)] = List()
        xs._1.indices.cross(xs._2.indices).foreach {
            case (i: Int, j: Int) => {
                scores = scores :+ (5-XStruct.compareTwo(xs._1(i),xs._2(j)), gt.contains((i,j)))
            }
        }

        println(scores)

        val PR: Map[Double, Double] = (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap
        output(PNG("/Users/ailyas/Downloads/", "test"), xyChart(
            PR.keys.toSeq.sorted -> (PR(_)),
            x = Axis("Recall", range = (0.0,1.05)),
            y = Axis("Precision", range = (0.0,1.05))
        ))
    }

    def runExperimentColumnLabel(): Unit = {

    }

    /*def runExperimentColumnLength(): Unit = {
        val lengths: Seq[Int] = List(100,200,500,1000,2500,5000,7000,10000)
        new XStruct().addNewLines(MicrobenchLoader.loadSpeedData().toArray) // Warm-up
        new XStruct().addNewLines(MicrobenchLoader.loadSpeedData().toArray) // Warm-up
        val speedData = MicrobenchLoader.loadSpeedData()
        val stats: Seq[Seq[Double]] = lengths.map(len => {
            val times: Seq[Long] = (1 to 30) map(_ => {
                val speedTest: Array[String] = speedData.toArray.slice(0,len)
                val initialTime: Long = System.nanoTime()
                new XStruct().addNewLines(speedTest)
                System.nanoTime() - initialTime
            })
            Utils.calcPercentiles(times, List(0.5,0.95,0.99)).map(Utils.nsToS)
        })
        output(PNG("graphs/", "mb_col_length_" + Utils.formattedDate()), xyChart(
            lengths.map(_.toDouble) -> stats.transpose.map(Y(_)),
            x = Axis("Column Length (tuples)", range=(0.0,10000.0)),
            y = Axis("Time (s)", range=(0.0,3.0))
        ))
    }*/

    /*def runExperimentNumHinges(): Unit = {
        val numDates: Seq[Int] = List(1,2,3,4,5)
        val stats: Seq[Seq[Double]] = numDates.map(nd => {
            val times = (1 to 30).map(_ => {
                val hingeTest: Array[String] = MicrobenchLoader.loadColData(nd).toArray
                val initialTime: Long = System.nanoTime()
                new XStruct().addNewLines(hingeTest).toString
                System.nanoTime() - initialTime
            })
            Utils.calcPercentiles(times, List(0.5, 0.95, 0.99)).map(Utils.nsToS)
        })
        output(PNG("graphs/", "mb_num_hinges" + Utils.formattedDate()), xyChart(
            numDates.map(_.toDouble*3-1) -> stats.transpose.map(Y(_)),
            x = Axis("Number of Hinges", range=(2.0,16.0)),
            y = Axis("Time (s)", range=(0.0,3.0))
        ))
    }*/

    def runExperiment(inputVals: Seq[Int], testLoader:Int=>Seq[String], imageName: String, xAxisTitle: String): Unit = {
        val stats: Seq[Seq[Double]] = inputVals.map(iv => {
            val times = (1 to 30).map(_ => {
                val test: Array[String] = testLoader(iv).toArray
                val initialTime: Long = System.nanoTime()
                new XStruct().addNewLines(test).toString
                System.nanoTime() - initialTime
            })
            Utils.calcPercentiles(times, List(0.5, 0.95, 0.99)).map(Utils.nsToS)
        })
        val xVals: Seq[Double] = inputVals.map(_.toDouble)
        val yVals: Seq[Double] = stats.flatten
        output(PNG("graphs/", imageName + Utils.formattedDate()), xyChart(
            xVals -> stats.transpose.map(Y(_)),
            x = Axis(xAxisTitle, range=(xVals.min,xVals.max)),
            y = Axis("Time (s)", range=(0.0,yVals.max*3.0))
        ))
    }

    def runExperimentMicrobench(runCL: Boolean, runNH: Boolean, runARL: Boolean): Unit = {
        if (runCL) runExperiment(
            List(100,200,500,1000,2500,5000,7000,10000),
            MicrobenchLoader.loadSpeedData,
            "mb_col_length",
            "Number of Tuples"
        )
        if (runNH) runExperiment(
            List(2,5,8,11,14),
            MicrobenchLoader.loadHingeData,
            "mb_num_hinges",
            "Number of Hinges"
        )
        if (runARL) runExperiment(
            List(1,5,10,25,50,75,100),
            MicrobenchLoader.loadAvgRowLengthData,
            "mb_avg_row_legth",
            "Average Row Length"
        )
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
