class SymbolStruct(_charHist: Map[Char,Long], _ccHist: Map[CharClass, List[Char]], _totalCount: Long) {
    val charHistogram : Map[Char, Long] = _charHist
    val ccHistogram : Map[CharClass, List[Char]] = _ccHist
    val totalCount : Long = _totalCount

    val representation: XClass = {
        val symbolPcts: Map[CharClass, Double] = ccHistogram.map(x => (x._1, x._2.length.toDouble/totalCount))
        val (maxSymbol: CharClass, maxPct: Double) = symbolPcts.maxBy(_._2)
        maxPct match {
            case x if x > 0.9 => if (!maxSymbol.isClass) maxSymbol.toXClass else {
                val truncatedHist: Map[Char,Long] = charHistogram.filterKeys((k: Char) => Utils.getCharacterClass(k) == maxSymbol)
                val totalHist: Map[Char,Long] = maxSymbol.baseHist ++ truncatedHist
                if (!Utils.significant(totalHist.values.toSeq)) maxSymbol.toXClass // Null hypothesis accepted
                else if (truncatedHist.size == 1) X_SPEC(truncatedHist.keys.head.toString) // Only one character
                else { // Multiple characters
                    val partialSums: Seq[(String, Long)] = truncatedHist.toSeq.sortBy(_._2).scanLeft(("", 0:Long))({
                        (old: (String, Long), c: (Char, Long)) => (c._1.toString, old._2 + c._2)
                    }).tail // Cumulative Sum
                    val cutoff: Long = ((1-Config.capturePct)*truncatedHist.values.sum).toLong
                    val charsToUse: Seq[String] = partialSums.filter(_._2 > cutoff).map(_._1)
                    X_OR(charsToUse.toList)
                }
            }
            case _ => if (!Utils.significant((Utils.asciiMap ++ charHistogram).values.toSeq)) X_ANY else maxSymbol.toXClass
        }
    }
    val symbolStringGenerator: Stream[String] = representation.lshDomain.toStream

    def this(c: Char) = this(
        Map[Char,Long](c -> 1),
        Map[CharClass,List[Char]](Utils.getCharacterClass(c)->List(c)),
        1:Long
    )

    def addChar(c: Char): SymbolStruct = {
        val _charHist: Map[Char, Long] = charHistogram + (c -> (charHistogram.getOrElse(c,0:Long) + 1:Long))
        val _ccHist: Map[CharClass, List[Char]] = {
            val charClass = Utils.getCharacterClass(c)
            ccHistogram + (charClass -> (ccHistogram.getOrElse(charClass, List()) :+ c))
        }
        val _totalCount: Long = totalCount + 1
        new SymbolStruct(_charHist, _ccHist, _totalCount)
    }

    def scoreChar(c: Char): Double = c match {
        case x if !(ccHistogram contains Utils.getCharacterClass(x)) => 1.0
        case x if !(charHistogram contains x) & !representation.isClass => 0.5
        case _ => 0.0
    }

    override def toString: String = representation.rep
}
