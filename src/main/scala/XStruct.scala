import org.saddle.Vec
import scala.util.control.Breaks._

class XStruct(_lines: List[String], _branches: Map[BranchStruct,Long], _bThresh: Double, _history: Seq[Double]) {
    private final val branches : Map[BranchStruct,Long] = _branches
    private final val lines : List[String] = if (Config.tts) _lines else List()
    private final val branchingThreshold : Double = _bThresh
    private final val history: Seq[Double] = if(Config.tts) _history else List()
    val tokensGenerator: Stream[String] = Utils.mergeStreams(branches.keys.toSeq.map(_.branchStringGenerator))
    val hingesGenerator: Stream[String] = Utils.mergeStreams(branches.keys.toSeq.map(_.tokenizerStringGenerator))
    val minHashStringGenerator: Stream[String] = Utils.mergeStreams(Seq(tokensGenerator, hingesGenerator ++ hingesGenerator))

    def this() = this(List[String](),Map[BranchStruct,Long](),Config.branchingSeed,List[Double]())

    def addNewLines(lines: Stream[String]): XStruct = if (Config.tts) {
        val scannedLines: Stream[(XStruct, Boolean)] = lines.filter(_.length>0).scanLeft((this,false)) {
            (x: (XStruct, Boolean), l: String) => x._1.addLine(l)
        }
        scannedLines.find(_._2).getOrElse(scannedLines.last)._1
    } else lines.filter(_.length>0).foldLeft((this,false)) {
            (x: (XStruct, Boolean), l: String) => x._1.addLine(l)
    }._1

    def addLine(line: String): (XStruct, Boolean)  = {
        val (_branches: Map[BranchStruct,Long], b: BranchStruct, score: Double) = findRightBranch(line)
        (new XStruct(
            lines :+ line,
            _branches + (b.learnString(line) -> (_branches(b) + 1.toLong)) - b,
            branchingThreshold,
            history :+ (if (_branches.size == branches.size) score else 0.0)
        ).trim(), (history.length + 1) % Config.inc == 0 && Config.neededSampleSize(Vec(history : _*).stdev) < history.length)
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
    private def trim(numTimes: Int = 1, _dm: Map[(BranchStruct,BranchStruct),Double] = null): XStruct = (if (branches.size <= Config.maxBranches) this else {
            val distanceMatrix: Map[(BranchStruct,BranchStruct),Double] = if (_dm == null) {
                val distanceMeasure = (x: (BranchStruct, BranchStruct)) => x._1.supersetScore(x._2)
                branches.keys.cross(branches.keys).map(
                    x => (x, distanceMeasure(x))
                ).toMap
            } else _dm

            val (minCoords: (BranchStruct, BranchStruct), minDist: Double) = distanceMatrix.filterKeys(x => x._1 != x._2).minBy(_._2)
            //println("Trimming")
            (new XStruct(
                lines,
                branches
                  + ((BranchStruct.merged _).tupled(minCoords) -> (branches(minCoords._1)+branches(minCoords._2)))
                  - minCoords._1 - minCoords._2,
                _bThresh = Math.max(minDist, branchingThreshold+0.01),
                List[Double]()
            ), distanceMatrix)
    }, numTimes) match {
        case ((x: XStruct, _), 1) => x
        case ((x: XStruct, y: Map[(BranchStruct,BranchStruct),Double]), _) => x.trim(numTimes-1, y)
    }


    def generateStrings: Stream[String] = {
        val randomIndex: Double = Math.random()
        val branchesAndFreqs: Array[(BranchStruct, Long)] = branches.toArray
        val randomBranch: Int = Vec(branchesAndFreqs.map(_._2) : _*) match {
            case x => (x.cumSum/x.sum).toSeq.zipWithIndex.find(_._1 > randomIndex).get._2
        }
        branchesAndFreqs(randomBranch)._1.generateRandomStrings(1).head
    } #:: generateStrings

    // Comparison
    def subsetScore(other: XStruct): Double = branches.map {
        case (b: BranchStruct, freq: Long) => freq*other.branches.map(_._1.supersetScore(b)).min
    }.sum/branches.values.sum.toDouble

    // Outlier score for a given string
    def computeOutlierScore(str: String): Double = branches.map {
        case (b: BranchStruct, c: Long) => b.scoreString(str) * (c.toDouble / branches.values.sum)
    }.sum

    def mergeWith(other: XStruct): XStruct = new XStruct(
        List(),
        this.branches ++ other.branches,
        0.0,
        List()
    ).trim(this.branches.size - Config.maxBranches)

    override def toString: String = branches.map(_.toString).mkString("|")

    implicit class Crossable[X](xs: Traversable[X]) {
        def cross[Y](ys: Traversable[Y]): Traversable[(X,Y)] = for { x <- xs; y <- ys } yield (x, y)
    }

    implicit def flatten1[A, B, C](t: ((A, B), C)): (A, B, C) = (t._1._1, t._1._2, t._2)
}

object XStruct {
    def compareTwo(x: XStruct, y: XStruct): Double = (x.subsetScore(y) + y.subsetScore(x))/2
    def mergeMultiple(xs: Seq[XStruct]): XStruct = xs.foldLeft(new XStruct())((z: XStruct, n: XStruct) => {
        z.mergeWith(n)
    })
}