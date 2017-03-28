import org.apache.commons.math3.stat.inference.ChiSquareTest

import scala.util.matching.Regex

/**
  * Created by ilyas on 2017-02-18.
  */
object Config {
    val maxBranches: Int = 10
    val branchingSeed: Double = 0.1
    val specChars: Regex = "[-~!@#$^%&*()_+={}\\\\[\\\\]|;:\\\"'`<,>.?/\\\\\\\\]".r
    val uppercaseChars: String = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    val lowercaseChars: String = "abcdefghijklmnopqrstuvwxyz"
    val numbers: String = "0123456789"
    val inc : Int = 10
    val capturePct: Double = 0.85
}

// Class for representing char classes in strings
sealed trait CharClass {
    def rep: String
    def isClass: Boolean
    def toXClass: XClass
    def domain: String
    def baseHist: Map[Char, Long] = domain.toCharArray.map((c:Char) => (c, 0:Long)).toMap
}
case object UCC extends CharClass { val (rep, isClass, toXClass, domain) = ("\\W", true, X_UCC, Config.uppercaseChars) }
case object LCC extends CharClass { val (rep, isClass, toXClass, domain) = ("\\w", true, X_LCC, Config.lowercaseChars) }
case object NUM extends CharClass { val (rep, isClass, toXClass, domain) = ("\\d", true, X_NUM, Config.numbers ) }
case class SPC(c: String) extends CharClass {
    assert(Config.specChars.findAllIn(c.toString).nonEmpty)
    val (rep, isClass, toXClass, domain) = (c, false, X_SPEC(c), c)
}

// Class for representing char classes in XStructures
sealed trait XClass { def rep: String }
case object X_UCC extends XClass { val rep = "\\W" }
case object X_LCC extends XClass { val rep = "\\w" }
case object X_NUM extends XClass { val rep = "\\d" }
case class X_SPEC(c: String) extends XClass { val rep: String = c.toString }
case class X_OR(cs: List[String]) extends XClass { val rep: String = "(" + cs.mkString("|") + ")" }


object Utils {
    // Returns the character class of a given character
    def getCharacterClass(c: Char): CharClass = c.toInt match {
        case x if 48 to 57 contains x => NUM
        case x if 65 to 90 contains x => UCC
        case x if 97 to 122 contains x => LCC
        case _ => SPC(c.toString)
    }

    // Performs Chi-Square test expecting a uniform distribution
    def significant(observed: Seq[Long]): Boolean = if (observed.length < 2) false else {
        val expectedFreq: Double = observed.sum.toDouble/observed.length
        val expected: Array[Double] = (1 to observed.length).map(_ => expectedFreq).toArray
        new ChiSquareTest().chiSquareTest(expected, observed.toArray,0.01)
    }
}