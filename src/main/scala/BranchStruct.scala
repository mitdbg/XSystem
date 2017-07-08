import java.util.regex.Pattern

import org.saddle._

import scala.util.matching.Regex

/**
  * Created by ilyas on 2017-02-18.
  * Immutable
  */
class BranchStruct(_l: Array[String], _tknzs:Array[String], _tks:Array[TokenStruct]) {
    val lines : Array[String] = _l
    val tokenStructs : Array[TokenStruct] = _tks
    val tokenizers : Array[String] = _tknzs
    checkRep()

    def this(s : String) =
        this(Array(s),BranchStruct.findTokenizers(s), BranchStruct.makeTokenStructs(s))

    def checkRep(): Unit = {
        if(tokenStructs.length != tokenizers.length) {
            println(tokenStructs, tokenizers)
            assert(false)
        }
        assert(tokenizers.last.equals("$"))
        assert(lines.nonEmpty)
    }

    def scoreString(str: String): Double = {
        val tokens = tokenizeString(str).padTo(tokenStructs.length,"")
        tokens.zip(tokenStructs).map((x: (String, TokenStruct)) => x._2.scoreToken(x._1)).sum/(str.length+0.01)
    }

    // Tokenize a string based on tokenizers
    def tokenizeString(str: String): Array[String] = {
        val suffixes = tokenizers.scanLeft(str)((s: String, t: String) => s.split(Regex.quote(t)) match {
            case x if x.nonEmpty => x.tail.mkString(t)
            case _ => ""
        })
        // Should be same length as tokenizers: "A/B/C$" -> "A/B/C$", "B/C$","C$" (no "" because of the init)
        (suffixes zip tokenizers) map ((s: (String, String)) => s._1.split(Regex.quote(s._2)) match {
            case x if x.nonEmpty => x.head
            case _ => ""
        })
    }

    // Learn a new string
    def learnString(str: String): BranchStruct = {
        checkRep()
        // if (!tokenizers.sameElements(BranchStruct.findTokenizers(str))) println("O NO") // TODO: Change this
        val strTokens: Array[String] = tokenizeString(str).padTo(tokenStructs.length, "")

        val newTokens: Array[TokenStruct] = (tokenStructs zip strTokens) map (
            (x: (TokenStruct, String)) => x._1.learnToken(x._2)
        )
        new BranchStruct(lines :+ str, tokenizers, newTokens)
    }

    // Generate random strings from this branch
    def generateRandomStrings(n: Int): List[String] = (1 to n).map(
        i => (tokenStructs.map(_.randomToken), tokenizers).zipped.map(_ + _).mkString("")
    ).toList

    // Check if this branch contains another branch
    def supersetScore(other: BranchStruct): Double = {
        var stdev: Double = Double.MaxValue
        var allScores: List[Double] = List()

        while (allScores.length < Config.neededSampleSize(stdev)) {
            allScores = allScores ++ other.generateRandomStrings(Config.inc).map(s => scoreString(s))
            stdev = Vec(allScores : _*).stdev
        }
        allScores.sum/(allScores.length+0.1) + 0.1
    }

    // Reset the done_adding counter for all of the tokens
    def reopened = new BranchStruct(lines,tokenizers,tokenStructs.map(_.reopened))

    override def toString: String = (tokenStructs.map(_.toString), tokenizers).zipped.map(_ + _).mkString("")
}

object BranchStruct {
    // Tokenizes in a string, returning hinges (called once for now, every iteration later)
    def findTokenizers(str: String): Array[String] =
        str.filter(s => Config.specChars.contains(s.toString)).toCharArray.map(_.toString) :+ "$"
        //(Config.specChars.findAllIn(str).toSeq :+ "$").toArray

    // Makes a set of initialized token structures (called once)
    def makeTokenStructs(str: String): Array[TokenStruct] =
        str.split("[" + Pattern.quote(Config.specChars) + "]",-1).map(x => new TokenStruct()).toSeq.toArray

    // Merges two branch structures
    def merged(outer: BranchStruct, inner: BranchStruct): BranchStruct =
        inner.lines.foldLeft(outer.reopened)((_b: BranchStruct, _s: String) => _b.learnString(_s))
}