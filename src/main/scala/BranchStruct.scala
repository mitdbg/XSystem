/**
  * Created by ilyas on 2017-02-18.
  * Mutable
  */
class BranchStruct(_l: Array[String], _tknzs:Array[String], _tks:Array[TokenStruct]) {
    val lines : Array[String] = _l
    val tokenStructs : Array[TokenStruct] = _tks
    val tokenizers : Array[String] = _tknzs
    checkRep()

    def this(s : String) =
        this(Array(s),BranchStruct.findTokenizers(s), BranchStruct.makeTokenStructs(s))

    def checkRep(): Unit = {
        assert(tokenStructs.length == tokenizers.length)
        assert(tokenizers.last.equals("$"))
        assert(lines.nonEmpty)
    }

    def scoreString(str: String): Double = {
        0.0
    }


    def tokenizeString(str: String): Array[String] = {
        val suffixes = tokenizers.scanLeft(str)((s: String, t: String) => s.split(t).tail.mkString(t))
        // Should be same length as tokenizers: "A/B/C$" -> "A/B/C$", "B/C$","C$" (no "" because of the init)
        (suffixes zip tokenizers) map ((s: (String, String)) => s._1.split(s._2).head)
    }

    // Learn a new string
    def learnString(str: String): BranchStruct = {
        checkRep()
        if (!tokenizers.sameElements(BranchStruct.findTokenizers(str))) println("O NO") // TODO: Change this
        val strTokens: Array[String] = tokenizeString(str)

        val newTokens: Array[TokenStruct] = (tokenStructs zip strTokens) map (
            (x: (TokenStruct, String)) => x._1.learnToken(x._2)
        )
        new BranchStruct(lines :+ str, tokenizers, newTokens)
    }

    override def toString: String = (tokenStructs.map(_.toString), tokenizers).zipped.map(_ + _).mkString("")
}

object BranchStruct {
    // Tokenizes in a string, returning hinges (called once for now, every iteration later)
    def findTokenizers(str: String): Array[String] =
        (Config.specChars.findAllIn(str).toSeq :+ "$").toArray

    // Makes a set of initialized token structures (called once)
    def makeTokenStructs(str: String): Array[TokenStruct] =
        str.split(Config.specChars.toString()).map(x => new TokenStruct()).toSeq.toArray
}