import java.util.Base64

import com.github.tototoshi.csv.CSVWriter
import org.sameersingh.scalaplot.Implicits._
import org.sameersingh.scalaplot.Style.{LineType, PointType}
import org.sameersingh.scalaplot.{Style, XYData, XYSeries}

/**
  * Created by ilyas on 2017-02-18.
  */
object Main {

    def main(args: Array[String]): Unit = {
        val (training, testing) = KDDLoader.loadData(Map(
            "trainNum" -> 200,
            "testNum" -> 500,
            "errorType" -> "snmpguess",
            "outlierProp" -> 0.5,
            "col" -> -1
        ))
        runExperimentOutliers(List(training), List(testing), "snmpguess")
        //runExperimentColCompare(FakeDataLoader)
        //runExperimentMicrobench(runCL = true, runNH = true, runARL = true)
    }

    def runExperimentOutliers(trainings: Seq[Stream[(List[String], String)]], testings: Seq[Stream[(List[String], String)]], name: String) : Unit = {
        assert(trainings.length == testings.length)
        val scores = (trainings, testings).zipped.flatMap(
            (training, testing) => {
                val x = new XStruct().addNewLines(training.map(_._1.mkString(",")))
                println(x.toString)
                println("Classifying...")
                val _scores = testing.map {
                    case (s: List[String], l: String) => (x.computeOutlierScore(s.mkString(",")), l == s"$name.")
                }.toList
                val filteredScores = _scores.filter(_._1 < 50.0)
                val maxScore: Double = filteredScores.map(_._1).sum/filteredScores.length
                _scores.map(x => (x._1/maxScore, x._2))
            }
        ).toList
        val PR: Map[Double, Double] = {
            (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap + (0.0 -> 1.0) + (1.0 -> 0.0)
        }

        val series: XYSeries = PR.keys.toSeq.sorted -> (PR(_))
        Utils.plotSeries(
            Map("" -> series),
            "graphs/",
            name,
            Axis("Recall", range = (0.0,1.01)),
            Axis("Precision", range = (0.0,1.01))
        )
    }

    def runExperimentColCompare(loader: ColCompareDataLoader): Unit = {
        val learnStructs = (s:Seq[Stream[String]]) => s.indices.map(i => {
            println(i)
            new XStruct().addNewLines(s(i))
        })
        val (table: Seq[Stream[String]], newParams: Map[String, Any]) = loader.loadData(
            Map(
                "length" -> 10,
                "numGroups" -> 3,
                "numCols" -> 10,
                "cols" -> List(
                    "public.bio_component_sequences.csv",
                    "public.biotherapeutics.csv",
                    "public.atc_classification.csv",
                    "public.cell_dictionary.csv",
                    "public.target_relations.csv",
                    "public.metabolism.csv",
                    "public.ligand_eff.csv",
                    "public.chembl_id_lookup.csv",
                    "public.target_type.csv",
                    "public.activity_stds_lookup.csv",
                    "public.action_type.csv",
                    "public.usan_stems.csv",
                    "public.component_go.csv",
                    "public.protein_class_synonyms.csv",
                    "public.component_synonyms.csv",
                    "public.bioassay_ontology.csv",
                    "public.molecule_dictionary.csv",
                    "public.compound_records.csv",
                    "public.assay_type.csv",
                    "public.component_class.csv"
                )
            )
        )
        val (positives: Set[(Int,Int)], negatives: Set[(Int, Int)]) = loader.loadGroundTruth(newParams + ("threshold" -> 0.1))
        val xs: Seq[XStruct] = learnStructs(table)
        var scores: List[(Double, Boolean)] = List()
        var _score: List[(Double, Boolean, (Int,Int))] = List()
        negatives.foreach {
            case (i: Int, j: Int) => {
                val score = (10-XStruct.compareTwo(xs(i),xs(j)), false, (i, j))
                scores = scores :+ (score._1, score._2)
                _score = _score :+ score
            }
        }
        positives.foreach {
            case (i: Int, j: Int) => {
                val score = (10-XStruct.compareTwo(xs(i),xs(j)), true, (i, j))
                scores = scores :+ (score._1, score._2)
                _score = _score :+ score
            }
        }

        val PR: Map[Double, Double] = {
            (0.0 until 100.0 by 0.01).map(calcPR(scores,_)).filter(x => !x._1.isNaN & !x._2.isNaN).toMap + (0.0 -> 1.0) + (1.0 -> 0.0)
        }

        val series: XYSeries = PR.keys.toSeq.sorted -> (PR(_))
        Utils.plotSeries(
            Map("" -> series),
            "graphs/",
            "column_sim_fake",
            Axis("Recall", range = (0.0,1.01)),
            Axis("Precision", range = (0.0,1.01))
        )
    }

    def runTimedExperiment(inputVals: Seq[Int], testLoader:Int=>Stream[String], imageName: String, xAxisTitle: String): Unit = {
        val stats: Seq[Seq[Double]] = inputVals.map(iv => {
            val times = (1 to 30).map(_ => {
                val test: Stream[String] = testLoader(iv)
                val initialTime: Long = System.nanoTime()
                new XStruct().addNewLines(test).toString
                System.nanoTime() - initialTime
            })
            Utils.calcPercentiles(times, List(0.5, 0.95, 0.99)).map(Utils.nsToS)
        })
        val xVals: Seq[Double] = inputVals.map(_.toDouble)
        val yVals: Seq[Double] = stats.flatten
        val seriesData: XYData = xVals -> stats.transpose.map(Y(_))
        Utils.plotSeries(
            Seq("50th %ile", "95th %ile", "99th %ile").zip(seriesData.serieses).toMap,
            "graphs/",
            imageName + Utils.formattedDate(),
            Axis(xAxisTitle, range=(xVals.min,xVals.max)),
            Axis("Time (s)", range=(0.0,yVals.max*3.0))

        )
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
