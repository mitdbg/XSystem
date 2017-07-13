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
  var struct : XStruct = null
  val log = Logging(context.system, this)
  val startTime: Long = System.nanoTime()
  var numReceived: Int = 0

  def receive = {

    case (x: XStruct) => {
      numReceived += 1
      struct = if(struct == null) {
        x
      } else {
        struct.mergeWith(x)
      }
      //structs = structs :+ x
      if(numReceived == numExpected) {
        //log.info("A " + System.nanoTime().toString)
        //val consolidated: XStruct = XStruct.mergeMultiple(structs)
        log.info("DONE " + System.nanoTime().toString + " " + struct.toString)
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