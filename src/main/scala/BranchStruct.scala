/**
  * Created by ilyas on 2017-02-18.
  * Mutable
  */
class BranchStruct(_l: Seq[String], _tknzs:Seq[String], _tks:Seq[TokenStruct]) {
    val lines : Seq[String] = _l
    val tokenStructs : Seq[TokenStruct] = _tks
    val tokenizers : Seq[String] = _tknzs
    checkRep()

    def this(s : String) =
        this(Seq(s),BranchStruct.findTokenizers(s), BranchStruct.makeTokenStructs(s))

    def checkRep(): Unit = {
        assert(tokenStructs.length == tokenizers.length)
        assert(tokenizers.last.equals("$"))
        assert(lines.nonEmpty)
    }

    def scoreString(str: String): Double = {
        0.0
    }

    // Learn a new string
    def learnString(str: String): BranchStruct = {
        checkRep()
        if (!tokenizers.equals(BranchStruct.findTokenizers(str))) println("O NO") // TODO: Change this
        val strTokens: Seq[String] = tokenizers.map(str.split(_).head)
        val newTokens: Seq[TokenStruct] = (tokenStructs zip strTokens) map (
            (x: (TokenStruct, String)) => x._1.learnToken(x._2)
        )
        new BranchStruct(lines :+ str, tokenizers, newTokens)
    }

    override def toString: String = Seq(tokenStructs).flatMap(_.zipWithIndex).sortBy(_._2).map(_._1).mkString("")
}

object BranchStruct {
    // Tokenizes in a string, returning hinges (called once for now, every iteration later)
    def findTokenizers(str: String): Seq[String] =
        Config.specChars.findAllIn(str).toSeq :+ "$"

    // Makes a set of initialized token structures (called once)
    def makeTokenStructs(str: String): Seq[TokenStruct] =
        str.split(Config.specChars.toString()).map(new TokenStruct(_)).toSeq
}