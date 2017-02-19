/**
  * Created by ilyas on 2017-02-18.
  */
class TokenStruct(token: String) {
    val symbolStructs : Seq[SymbolStruct] = null
    val doneAdding : Boolean = false

    def learnToken(token: String): TokenStruct = if (doneAdding) this else {
        null
    }
}
