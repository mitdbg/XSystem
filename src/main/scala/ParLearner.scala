import akka.actor.{Actor, ActorRef, Props}
import akka.event.Logging

object ParLearner {
  def props(consolidator: ActorRef): Props = Props(new ParLearner(consolidator))
}

class ParLearner(consolidator: ActorRef) extends Actor {
  val log = Logging(context.system, this)

  def receive = {
    case (lines: Stream[String]) => {
      log.info(System.nanoTime().toString)
      val x = new XStruct().addNewLines(lines)
      consolidator ! x
      context.stop(self)
    }
    case _ => log.info("ERROR")
  }
}

object Consolidator {
  def props(numExpected: Int): Props = Props(new Consolidator(numExpected))
}

class Consolidator(numExpected: Int) extends Actor {
  var structs : List[XStruct] = List()
  val log = Logging(context.system, this)
  val startTime: Long = System.nanoTime()

  def receive = {
    case (x: XStruct) => {
      structs = structs :+ x
      if(structs.length == numExpected) {
        log.info("A " + System.nanoTime().toString)
        val consolidated: XStruct = XStruct.mergeMultiple(structs)
        log.info(System.nanoTime().toString)
        context.stop(context.actorOf(Props.empty, "killer"))
      }
    }
    case _ => log.info("UNEXPECTED TYPE")
  }
}

object Printer {
  def props: Props = Props(new Printer)
}

class Printer extends Actor {
  val log = Logging(context.system, this)
  def receive = {
    case (x: Seq[String]) => x.foreach(log.info)
    case _ => log.info("POOP")
  }
}