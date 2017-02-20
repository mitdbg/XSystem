import org.saddle.io

class XStruct {
    var branches : Array[BranchStruct] = Array[BranchStruct]()
    var branchDistribution : Array[Long] = Array[Long]()

    var lines : Array[String] = Array[String]()
    var branchingThreshold : Double = Config.branchingSeed

    def addNewLines(lines: Array[String]): Unit = {
        lines.map((s: String) => {
            findRightBranch(s)
        })
    }

    // Given a string, find which branch of the representation should learn it
    def findRightBranch(str: String): Int = {
        val scores: IndexedSeq[(Int,Double)] = branches.indices.map((x: Int) => (x, branches(x).scoreString(str)))
        val minPair : (Int, Double) = scores.minBy(_._2)
        if (minPair._2 < branchingThreshold) minPair._1 else {
            val b: BranchStruct = new BranchStruct(str)
            branches = branches :+ b // Appends a new branch
            branches.length-1
        }
    }

    // Outlier score for a given string
    def computeOutlierScore(str: String): Double = {
        0.0
    }

    override def toString: String = branches.map(_.toString).mkString("|")
}
