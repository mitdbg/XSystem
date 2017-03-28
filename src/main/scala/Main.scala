/**
  * Created by ilyas on 2017-02-18.
  */
object Main {
    def main(args: Array[String]): Unit = {
        val x = new XStruct()
        x.addNewLines(Array(
            "11/06/1999",
            "11/06/1999",
            "12/06/1999",
            "41/06/1999",
            "31/06/1999",
            "14/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999",
            "11/06/1999"
        ))
        x.addNewLines(Array(
            "Andrew",
            "Andrew",
            "Andrew",
            "Andrew",
            "Andrew",
            "Andrew"
        ))
        println(x.toString)
    }

    def runExperimentOutliers(): Unit = {

    }

    def runExperimentColCompare(): Unit = {

    }

    def runExperimentColumnLabel(): Unit = {

    }
}
