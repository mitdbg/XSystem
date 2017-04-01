import org.saddle.{Frame, Series}
import org.saddle.io.{CsvFile, CsvParser}

/**
  * Created by ilyas on 2017-02-18.
  */
object Main {

    def main(args: Array[String]): Unit = {
        val (training, testing) = Loader.loadCarData()
        val x = new XStruct()
        val newX = x.addNewLines(training.map(_._1).toArray)
        println(newX.toString)
        testing.foreach {
            case (s: String, l: String) => println(newX.computeOutlierScore(s) + " " + l)
        }
    }

    def runExperimentOutliers(): Unit = {

    }

    def runExperimentColCompare(): Unit = {

    }

    def runExperimentColumnLabel(): Unit = {

    }
}

object Loader {
    val carPath = "/Users/ailyas/Documents/Datasets/TwoClassUCI/Car/car.csv"

    def formatFrame(f: Frame[Int,Int,String]): List[(String,String)] = f.colSlice(0,f.numCols)
      .rreduce(_.toSeq.map(_._2).mkString(","))
      .toSeq
      .map(_._2)
      .toList
      .zip(
          f.colAt(f.numCols-1)
            .toSeq
            .map(_._2)
      )

    def loadCarData(): (List[(String,String)],List[(String,String)]) = {
        val carData: Frame[Int,Int,String] = CsvParser.parse(CsvFile(carPath))
        val badCars: Frame[Int,Int,String] = carData.rfilter(
            (x: Series[Int, String]) => x.last.toString.equals("unacc")
        )
        val goodCars: Frame[Int,Int,String] = carData.rfilter(
            (x: Series[Int, String]) => !x.last.toString.equals("unacc")
        )

        val training:List[(String,String)] = formatFrame(goodCars.rowSlice(0,(goodCars.numRows*0.8).toInt))
        val testing:List[(String,String)] = formatFrame(goodCars.rowSlice((goodCars.numRows*0.8).toInt, goodCars.numRows)
          .rjoin(badCars.rowSlice(0,100)))
        (training, testing)
    }
}