import console._
import java.io.File

object Main extends App {
  val shell= new Shell(new File("/"))
  shell.repl()
}

