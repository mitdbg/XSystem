import org.apache.commons.math3.stat.inference.ChiSquareTest

import scala.util.matching.Regex

/**
  * Created by ilyas on 2017-02-18.
  */
object Config {
    val maxBranches: Int = 7
    val branchingSeed: Double = 0.1
    val specChars: Regex = "[-~!@#$^%&*()_+={}\\\\[\\\\]|;:\\\"'`<,>.?/\\\\\\\\]".r
    val uppercaseChars: Set[Char] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray.toSet
    val lowercaseChars: Set[Char] = "abcdefghijklmnopqrstuvwxyz".toCharArray.toSet
    val numbers: Set[Char] = "0123456789".toCharArray.toSet
    val inc : Int = 10
    val capturePct: Double = 0.85

    def neededSampleSize(std: Double): Double = Math.pow(1.96*std/0.5, 2.0)
}

object Utils {
    // Returns the character class of a given character
    def getCharacterClass(c: Char): CharClass = c.toInt match {
        case x if 48 to 57 contains x => NUM
        case x if 65 to 90 contains x => UCC
        case x if 97 to 122 contains x => LCC
        case _ => SPC(c.toString)
    }

    // Performs Chi-Square test expecting a uniform distribution
    def significant(observed: Seq[Long]): Boolean = if (observed.length < 2) false else
        new ChiSquareTest().chiSquareTest(Array.fill(observed.length)(1.0), observed.toArray,0.01)

    // Potentially clean up later?
    def asciiMap: Map[Char,Long] = (0 to 256).map(i => (i.toChar,0:Long)).toMap
}