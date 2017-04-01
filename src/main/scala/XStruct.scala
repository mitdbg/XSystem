class XStruct(_lines: List[String], _branches: Map[BranchStruct,Long]) {
    private final val branches : Map[BranchStruct,Long] = _branches
    private final val lines : List[String] = _lines
    private final val branchingThreshold : Double = Config.branchingSeed

    def this() = this(List[String](),List[BranchStruct](),List[Long]())

    def addNewLines(lines: Array[String]): XStruct = lines.foldLeft(this)(
        (x:XStruct, l:String) => x.addLine(l)
    )

    def addLine(line: String): XStruct = {
        val ((_branches: List[BranchStruct], _dist: List[Long]), ind: Int) = findRightBranch(line)
        val b = if (ind == -1) _branches.length-1 else ind
        new XStruct(
            lines :+ line,
            _branches.updated(b, _branches(b).learnString(line)),
            _dist.updated(b,_dist(b)+1)
        )
    }

    private def newBranch(str: String): (Map[BranchStruct,Long], BranchStruct) = {
        val b = new BranchStruct(str)
        (branches + (b -> (0:Long)), b)
    }

    // Given a string, find which branch of the representation should learn it
    private def findRightBranch(str: String): (Map[BranchStruct,Long],BranchStruct) = if (branches.isEmpty) newBranch(str) else {
        val scores: Iterable[(Double,BranchStruct)] = branches.keys.map(_.scoreString(str)).zip(branches.keys)
        val minPair : (Double, BranchStruct) = scores.minBy(_._1)
        if (minPair._1 < branchingThreshold) (branches, minPair._2) else newBranch(str)
    }

    // Merge back
    private def trim(): XStruct = {
        val distanceMeasure = (x: (Int, Int)) => branches(x._1).supersetScore(branches(x._2))
        val distanceMatrix: Map[(Int,Int),Double] = branches.indices.cross(branches.indices).map(x => (x, distanceMeasure(x))).toMap
        val (minCoords: (Int, Int), minDist: Double) = distanceMatrix.filterKeys(x => x._1 != x._2).minBy(_._2)
    }

    // Outlier score for a given string
    def computeOutlierScore(str: String): Double = branches.indices.map(
        (i: Int) => branches(i).scoreString(str)*(branchDist(i).toDouble/branchDist.sum)
    ).sum/branches.length.toDouble

    override def toString: String = branches.map(_.toString).mkString("|")

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]): Traversable[(X,Y)] = for { x <- xs; y <- ys } yield (x, y)
    }
}
