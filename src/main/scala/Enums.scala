import scala.util.Random

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
  val realC: String = if(!Config.specChars.contains(c.toString)) {
    println(c.toString)
    "*"
  } else c
  val (rep, toXClass, domain) = (realC, X_SPEC(realC), realC.toCharArray.toSet)
  override val isClass = false
}

// Class for representing char classes in XStructures
sealed trait XClass {
  def rep: String
  def domain: Iterable[Char]
  def isClass: Boolean = true
  def randomRep: String = domain.toList(Random.nextInt(domain.size)).toString
  def lshDomain: Set[String] = Set(rep)
  override def toString: String = rep
}
case object X_UCC extends XClass { val (rep, domain) = ("\\W", Config.uppercaseChars) }
case object X_LCC extends XClass { val (rep, domain) = ("\\w", Config.lowercaseChars) }
case object X_NUM extends XClass { val (rep, domain) = ("\\d", Config.numbers) }
case object X_ANY extends XClass { val (rep, domain) = ("*",  Config.numbers ++ Config.lowercaseChars ++ Config.uppercaseChars) }
case class X_SPEC(c: String) extends XClass {
  val rep: String = c.toString
  val domain: Iterable[Char] = Set(c.charAt(0))
  override val isClass = false
}
case class X_OR(cs: List[String]) extends XClass {
  val rep: String = "(" + cs.mkString("|") + ")"
  val domain: Iterable[Char] = cs.map(_.charAt(0))
  override val isClass = false
  override def lshDomain: Set[String] = domain.map((d: Char) => d.toString).toSet
}
