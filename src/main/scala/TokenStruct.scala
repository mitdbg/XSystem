/**
  * Created by ilyas on 2017-02-18.
  */
import scala.language.postfixOps

import org.saddle._

class TokenStruct(_symbolStructs : Seq[SymbolStruct] = Seq(),
                  _doneAdding : Boolean = false,
                  _history : Seq[Double] = Seq(),
                  _stdDev : Double = 100.0,
                  _archive : Seq[Double] = Seq()
                 )
{
  private final val symbolStructs : Seq[SymbolStruct] = _symbolStructs
  private final val doneAdding : Boolean = _doneAdding
  private final val history : Seq[Double] = _history
  private final val stdDev : Double = _stdDev
  val archive : Seq[Double] = _archive

  val scoreSoFar: Double = archive.sum/archive.length
  val representation: Seq[XClass] = symbolStructs.map(_.representation)

  def learnToken(token: String): TokenStruct = if (doneAdding) this else {
      val tokenScore : Double = symbolStructs.zip(token).map((c: (SymbolStruct, Char)) => c._1.scoreChar(c._2)).sum

      val _symbolStructs : Seq[SymbolStruct] = (token zipWithIndex) map (
              (ci : (Char, Int)) => {
                  if (ci._2 >= symbolStructs.length) new SymbolStruct(ci._1)
                  else symbolStructs(ci._2).addChar(ci._1)
              }
          )
      val _history : Seq[Double] = history :+ tokenScore
      val _stdDev : Double = if (_history.length % Config.inc == 0) Vec(_history : _*).stdev else stdDev
      val _doneAdding : Boolean = Config.neededSampleSize(_stdDev) < _history.length
      if (_doneAdding) {
        print("HI")
      }
      val _archive = if (_doneAdding) archive ++ _history else archive
      new TokenStruct(_symbolStructs, _doneAdding, _history, _stdDev, _archive)
  }

  def reopened: TokenStruct = new TokenStruct(symbolStructs,false,_archive=archive++history)

  def scoreToken(token: String): Double = token.toCharArray.zip(symbolStructs).map {
      (cs: (Char, SymbolStruct)) => cs._2.scoreChar(cs._1)
  }.sum + (token.length - symbolStructs.length match {
      case 0 => 0
      case x if x > 0 => x
      case _ => 1.0
  })

  def randomToken: String = representation.map(_.randomRep).mkString("")

  override def toString: String = representation.map(_.toString).mkString("")
}
