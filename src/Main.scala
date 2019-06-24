import console._
import java.io.File

object Main extends App {
  val shell= new Shell(new File("/"), Seq(), Map("ll" -> "ls"))
  shell.repl()
}

