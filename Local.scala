

import akka.actor._
import com.typesafe.config.ConfigFactory
import java.io.File

case class Assign(prefix: String)
case class KickOff(hi: String)
case class Done(prefix: String)

case class AssignBlock(prefix: String, start: Int)


/**
 * @author chand
 */
object Local extends App{
  
val configFile = getClass.getClassLoader.getResource("localapplication.conf").getFile
  val config = ConfigFactory.parseFile(new File(configFile))
  val system = ActorSystem("ClientSystem",config)
  val localActor = system.actorOf(Props[LocalActor], name="local")
  var zeroCount = 0
  val ipAddress = args(0)
  def md = java.security.MessageDigest.getInstance("SHA-256")
  val UnitSize = 31

  localActor ! "START" 
   
class LocalActor extends Actor {

  // create the remote actor
  val remote = context.actorFor("akka.tcp://HelloRemoteSystem@"+ipAddress+ ":5150/user/RemoteActor")
  //var counter = 0
  val regex = """(\w+)\s+(\w+)\s+(\w+)\s+(\d+)""".r
  def receive = {
    case "START" => 
        remote ! "Hello from the LocalActor"
    
    case msg: String => 
      msg match{
        case regex(a,b,c,d) =>
           println("count received is: "+ d)
	   zeroCount = d.toInt
           def str = "chandravikas"
           def md = java.security.MessageDigest.getInstance("SHA-256")
           def UnitSize = 20
  
           localActor ! "Started the process from client 1"
    //println("Started the process")
           val _system = ActorSystem("BossActor")
           val master = _system.actorOf(Props[BossActor], name = "BossActor")

           localActor ! "Created Boss Actor from client1"

           var init = KickOff("Hey")
           master ! init
        case _ =>
           remote ! msg
      }
      
        
      
  }
}

  

  class BossActor extends Actor {

    var count = 1

    def spawnOff(prefix: String) = {
      var startAscii = 53;
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
        var ascii = 53
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
          val sha = toSHA256(prefix + nonString)
          if (sha.startsWith("0000")) {
            //println("BitCoin " + " : " + str + nonString + " " + sha)
          }
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

    def toSHA256(str: String): String = {
      val sha1 = md.digest(str.getBytes("UTF-8")).map("%02x".format(_)).mkString("")
      var targetString = new StringBuilder()
      for(i<- 1 to zeroCount){
         targetString.append("0")
      }
      if (sha1.startsWith(targetString)) {
        localActor ! "BitCoin " + " : " + str + " " + sha1
      }
      return "" 
    }
  }
}
