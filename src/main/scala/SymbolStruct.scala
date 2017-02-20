class SymbolStruct(initialChar : Char) {
    val histogram : Map[Char, Int] = Map()
    val representation: String = {
        "A"
    }

    def addChar(c: Char): SymbolStruct = null
    def scoreChar(c: Char): Double = 0.0

    override def toString: String = representation
}
