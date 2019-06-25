import java.io.File

import console.Shell

object Main extends App {
  val shell= new Shell(new File("/"), Seq(), Map("ll" -> "ls"))
  shell.repl()
}
