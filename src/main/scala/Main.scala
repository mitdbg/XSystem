import org.sameersingh.scalaplot.Implicits._

/**
  * Created by ilyas on 2017-02-18.
  */
object Main {

    def main(args: Array[String]): Unit = {
        //val (training, testing) = ForestCoverLoader.loadData(Map(
        //    "trainPct" -> 0.9
        //))
        //runExperimentOutliers(training, testing, "outliers_1")
        runExperimentColCompare(ChemblDataLoader)
        //runExperimentMicrobench(runCL = true, runNH = true, runARL = false)

    }

    def runExperimentOutliers(training: List[(List[String], String)], testing: List[(List[String], String)], name: String) : Unit = {
        val x = new XStruct().addNewLines(training.map(_._1.mkString(",")).toArray)
        println(x.toString)
        println("Classifying...")
        val scores = testing.map {
            case (s: List[String], l: String) => (x.computeOutlierScore(s.mkString(",")), l == s"$name.")
        }
        println("Analyzing...")
        val PR: Map[Double, Double] = (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap
        output(PNG("graphs/", name), xyChart(
            PR.keys.toSeq.sorted -> (PR(_)),
            x = Axis("Recall", range = (0.0,1.05)),
            y = Axis("Precision", range = (0.0,1.05))
        ))
    }

    def runExperimentColCompare(loader: ColCompareDataLoader): Unit = {
        val learnStructs = (s:List[List[String]]) => s.map(x => new XStruct().addNewLines(x.toArray))
        val (table: List[List[String]], newParams: Map[String, Any]) = loader.loadData(
            Map(
                "length" -> 10,
                "numGroups" -> 3,
                "numCols" -> 10,
                "cols" -> List(
                    "public.action_type.csv",
                    "public.activity_stds_lookup.csv",
                    "public.assay_parameters.csv",
                    "public.assay_type.csv",
                    "public.assays.csv",
                    "public.atc_classification.csv",
                    "public.binding_sites.csv",
                    "public.bio_component_sequences.csv",
                    "public.bioassay_ontology.csv",
                    "public.biotherapeutic_components.csv",
                    "public.biotherapeutics.csv"
                )
            )
        )
        val gt: Set[(Int,Int)] = loader.loadGroundTruth(newParams + ("threshold" -> 0.5))
        val xs: List[XStruct] = learnStructs(table)
        var scores: List[(Double, Boolean)] = List()
        xs.indices.cross(xs.indices).foreach {
            case (i: Int, j: Int) => {
                if (i < j) scores = scores :+ (10-XStruct.compareTwo(xs(i),xs(j)), gt.contains((i,j)))
            }
        }

        println(scores.mkString("\n"))

        val PR: Map[Double, Double] = (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap
        output(PNG("graphs/", "column_sim"), xyChart(
            PR.keys.toSeq.sorted -> (PR(_)),
            x = Axis("Recall", range = (0.0,1.05)),
            y = Axis("Precision", range = (0.0,1.05))
        ))
    }

    def runTimedExperiment(inputVals: Seq[Int], testLoader:Int=>Seq[String], imageName: String, xAxisTitle: String): Unit = {
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
        if (runCL) runTimedExperiment(
            List(100,200,500,1000,2500,5000,7000,10000),
            MicrobenchLoader.loadSpeedData,
            "mb_col_length",
            "Number of Tuples"
        )
        if (runNH) runTimedExperiment(
            List(2,5,8,11,14),
            MicrobenchLoader.loadHingeData,
            "mb_num_hinges",
            "Number of Hinges"
        )
        if (runARL) runTimedExperiment(
            List(1,5,10,25,50,75,100),
            MicrobenchLoader.loadAvgRowLengthData,
            "mb_avg_row_length",
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
