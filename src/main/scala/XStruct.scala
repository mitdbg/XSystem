import org.saddle.Vec
import scala.util.control.Breaks._

class XStruct(_lines: List[String], _branches: Map[BranchStruct,Long], _bThresh: Double, _history: Seq[Double]) {
    private final val branches : Map[BranchStruct,Long] = _branches
    private final val lines : List[String] = _lines
    private final val branchingThreshold : Double = _bThresh
    private final val history: Seq[Double] = _history

    def this() = this(List[String](),Map[BranchStruct,Long](),Config.branchingSeed,List[Double]())

    def addNewLines(lines: Array[String]): XStruct = lines.foldLeft(this) {
        (x: XStruct, l: String) => {
            val (newX: XStruct, shouldStop: Boolean) = x.addLine(l)
            if (!shouldStop)
                newX
            else
                return newX
        }
    }

    def addLine(line: String): (XStruct, Boolean)  = {
        val (_branches: Map[BranchStruct,Long], b: BranchStruct, score: Double) = findRightBranch(line)
        (new XStruct(
            lines :+ line,
            _branches + (b.learnString(line) -> (_branches(b) + 1.toLong)) - b,
            branchingThreshold,
            history :+ score
        ).trim, (history.length + 1) % Config.inc == 0 && Config.neededSampleSize(Vec(history : _*).stdev) < history.length)
    }

    private def newBranch(str: String): (Map[BranchStruct,Long], BranchStruct) = {
        val b = new BranchStruct(str)
        (branches + (b -> (0:Long)), b)
    }

    // Given a string, find which branch of the representation should learn it
    private def findRightBranch(str: String): (Map[BranchStruct,Long],BranchStruct,Double) = if (branches.isEmpty) (newBranch(str), 0.0) else {
        val scores: Iterable[(Double,BranchStruct)] = branches.keys.map(_.scoreString(str)).zip(branches.keys)
        val minPair : (Double, BranchStruct) = scores.minBy(_._1)
        if (minPair._1 < branchingThreshold) (branches, minPair._2, minPair._1) else (newBranch(str), minPair._1)
    }

    // Merge back
    private def trim: XStruct = if (branches.size <= Config.maxBranches) this else {
        val distanceMeasure = (x: (BranchStruct, BranchStruct)) => x._1.supersetScore(x._2)
        val distanceMatrix: Map[(BranchStruct,BranchStruct),Double] = branches.keys.cross(branches.keys).map(
            x => (x, distanceMeasure(x))
        ).toMap
        val (minCoords: (BranchStruct, BranchStruct), minDist: Double) = distanceMatrix.filterKeys(x => x._1 != x._2).minBy(_._2)
        //println("Trimming")
        new XStruct(
            lines,
            branches
              + ((BranchStruct.merged _).tupled(minCoords) -> (branches(minCoords._1)+branches(minCoords._2)))
              - minCoords._1 - minCoords._2,
            _bThresh = Math.max(minDist, branchingThreshold+0.01),
            List[Double]()
        )
    }

    // Comparison
    def subsetScore(other: XStruct): Double = branches.map {
        case (b: BranchStruct, freq: Long) => freq*other.branches.map(_._1.supersetScore(b)).min
    }.sum/branches.values.sum.toDouble

    // Outlier score for a given string
    def computeOutlierScore(str: String): Double = branches.map {
        case (b: BranchStruct, c: Long) => b.scoreString(str) * (c.toDouble / branches.values.sum)
    }.sum

    override def toString: String = branches.map(_.toString).mkString("|")

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]): Traversable[(X,Y)] = for { x <- xs; y <- ys } yield (x, y)
    }

    implicit def flatten1[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)
}

object XStruct {
    def compareTwo(x: XStruct, y: XStruct): Double = (x.subsetScore(y) + y.subsetScore(x))/2
}