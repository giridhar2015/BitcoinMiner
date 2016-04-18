
import akka.actor._
import com.typesafe.config.ConfigFactory

import java.io.File

case class AssignBlock(prefix: String, start: Int)
case class KickOff(hi: String)
case class Done(prefix: String)

object HelloRemote extends App  {
 def str = "chandravikas"
  def md = java.security.MessageDigest.getInstance("SHA-256")
  def UnitSize = 50
  var baseTime = System.currentTimeMillis()
  var count = 0

  val configFile = getClass.getClassLoader.getResource("Remoteapplication.conf").getFile
  val config = ConfigFactory.parseFile(new File(configFile))
  val system = ActorSystem("HelloRemoteSystem", config)
  val remoteActor = system.actorOf(Props[RemoteActor], name = "RemoteActor")
  val message = "Prepare args " + args(0)
  remoteActor ! message
  remoteActor ! "Server mining"

  def master = system.actorOf(Props[BossActor], name = "BossActor")
  var init = KickOff("Hey")
  master ! init

  println("The RemoteActor is alive. Hurray!!")

  class RemoteActor extends Actor {

    val regex = """(\w+)\s+(\w+)\s(\d+)""".r
    def receive = {

      case "Hello from the LocalActor" =>

        val message = "Start mining with " + count
        sender ! message

      case msg: String =>
        msg match {
          case regex(a, b, c) =>
            println(a + " " + b + " " + c)
            count = c.toInt

          case _ =>
            println(s"RemoteActor received message '$msg'")

        }
    }
  }

  class BossActor extends Actor {

    var count = 1

    def spawnOff(prefix: String) = {
      var startAscii = 33;
      var preStr = ""
      while (startAscii < 94) {
        var worker = context.actorOf(Props[WorkerActor], name = "WorkerActor" + count.toString())
        count = count + 1
        worker ! AssignBlock(prefix, startAscii)
        startAscii = startAscii + UnitSize
      }
    }

    def receive: Receive = {
      case KickOff(hi: String) => {
        spawnOff("chandravikas")
      }

      case Done(prefix: String) => {
        var ascii = 33
        while (ascii < 94) {
          spawnOff(prefix + ascii.toChar)
          ascii = ascii + 1
        }
      }
    }

  }

  class WorkerActor extends Actor {

    def receive: Receive = {
      case AssignBlock(prefix: String, start: Int) => {
        var ascii = start
        var update = true
        while (ascii < start + UnitSize && ascii < 94) {
          val nonString = ascii.toChar
          toSHA256(prefix + nonString, context.parent)
          ascii = ascii + 1
        }

        if (ascii == 94) {
          var done = Done(prefix);
          context.parent ! done
        }
        context.stop(self)
      }

      case _ => {
        println("Yolo")
      }
    }

    def toSHA256(str: String, parent: ActorRef) = {
      val sha1 = md.digest(str.getBytes("UTF-8")).map("%02x".format(_)).mkString("")
      val sb = new StringBuilder()
      for(i<- 1 to count){
        sb.append("0")
      }
      if (sha1.startsWith(sb)) {
        val time = System.currentTimeMillis() - baseTime
        println("BitCoin Found" + " : " + str + " " + sha1 + "  in " + time + "seconds")
      }
    }
  }


}

