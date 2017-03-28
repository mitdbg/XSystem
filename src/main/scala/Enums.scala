/**
  * Created by ailyas on 3/28/17.
  */
// Class for representing char classes in strings
sealed trait CharClass {
  def rep: String
  def toXClass: XClass
  def domain: Set[Char]
  def isClass: Boolean = true
  def baseHist: Map[Char, Long] = domain.map((c:Char) => (c, 0:Long)).toMap
}
case object UCC extends CharClass { val (rep, toXClass, domain) = ("\\W", X_UCC, Config.uppercaseChars) }
case object LCC extends CharClass { val (rep, toXClass, domain) = ("\\w", X_LCC, Config.lowercaseChars) }
case object NUM extends CharClass { val (rep, toXClass, domain) = ("\\d", X_NUM, Config.numbers ) }
case class SPC(c: String) extends CharClass {
  assert(Config.specChars.findAllIn(c.toString).nonEmpty)
  val (rep, toXClass, domain) = (c, X_SPEC(c), c.toCharArray.toSet)
  override val isClass = false
}

// Class for representing char classes in XStructures
sealed trait XClass {
  def rep: String
  def isClass: Boolean = true
  override def toString: String = rep
}
case object X_UCC extends XClass { val rep = "\\W" }
case object X_LCC extends XClass { val rep = "\\w" }
case object X_NUM extends XClass { val rep = "\\d" }
case object X_ANY extends XClass { val rep = "*" }
case class X_SPEC(c: String) extends XClass {
  val rep: String = c.toString
  override val isClass = false
}
case class X_OR(cs: List[String]) extends XClass {
  val rep: String = "(" + cs.mkString("|") + ")"
  override val isClass = false
}
