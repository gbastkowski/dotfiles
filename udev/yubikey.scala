import sys.process._

object Main extends App {

  println(
  sendNotification("sadf").!
  )

  def sendNotification(message: String) = {
    val environment = xwaylandEnvironment.toList.map {
      case (key, value) => key + "=" + "\\\"" + value + "\\\""
    }.mkString(" ")

    val cmd = s"""$environment notify-send -t 2000 \\"YubiKey\\" \\"$message\\""""
    s"""/bin/su $activeUser -c "$cmd" """
  }

  def xwaylandEnvironment = activePidAndDisplay match {
    case (pid, display) =>
      scala.io.Source.fromFile(s"/proc/$pid/environ").getLines.toList.head
        .split("\u0000").toList
        .map(_.split("=").toList)
        .map { case key :: value :: _ => key -> value }
        .toMap
  }

  def activePidAndDisplay = {
    getStdOut("ps -A -o user= -o pid= -o cmd=")
      .split("\n").toList
      .filter { s =>
        s.startsWith(activeUser) && s.contains("Xwayland")
      }
      .head.split("\\s+").toList match {
        case user :: pid :: cmd :: display :: _ => (pid, display)
      }
  }

  def activeUser = {
    val activeTty = scala.io.Source.fromFile("/sys/class/tty/tty0/active").getLines.toList.head

    getStdOut("who").split("\n").toList.map(_.split("\\s+").toList)
      .collect {
        case user :: tty :: _ if tty == activeTty => user
      }
      .head
  }

  def getStdOut(cmd: String) = {
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val exitCode = cmd ! ProcessLogger(s => stdout append (s + "\n"), stderr append _)
    stdout.toString
  }
}
