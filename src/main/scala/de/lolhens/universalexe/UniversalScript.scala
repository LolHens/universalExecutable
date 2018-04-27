package de.lolhens.universalexe

import Shell._
import Value._
import Const._

object UniversalScript {

  abstract class Script[Self <: Script[Self]](val string: String) {
    def withString(string: String): Self
    def newline: String
    def ++(script: Self): Self = withString(string + newline + script.string)
  }

  case class ShellScript(override val string: String) extends Script[ShellScript](string) {
    override def withString(string: String): ShellScript = ShellScript(string)
    override def newline: String = "\n"
  }

  case class CmdScript(override val string: String) extends Script[CmdScript](string) {
    override def withString(string: String): CmdScript = CmdScript(string)
    override def newline: String = "\r\n"
  }

  case class UniversalScript(shellScript: ShellScript,
                             cmdScript: CmdScript) {
    def string: String = UniversalScript.universalScript(shellScript.string, cmdScript.string, shebang = true)
    def ++(universalScript: UniversalScript): UniversalScript = UniversalScript(
      shellScript ++ universalScript.shellScript,
      cmdScript ++ universalScript.cmdScript
    )
  }

  object UniversalScript {
    private def universalScript(shellCommands: String,
                                cmdCommands: String,
                                shebang: Boolean): String = {
      Seq(
        if (shebang) "#!/usr/bin/env sh" else "",
        "@ 2>/dev/null # 2>nul & echo off & goto BOF\r",
        ":",
        shellCommands.replaceAll("\r\n|\n", "\n"),
        "exit",
        Seq(
          "",
          ":BOF",
          cmdCommands.replaceAll("\r\n|\n", "\r\n"),
          "exit /B %errorlevel%",
          ""
        ).mkString("\r\n")
      ).filterNot(_.isEmpty).mkString("\n")
    }
  }



  def unpackBlock(blockName: String, outPath: String): UniversalScript = {
    val blockStart = s"<$blockName>"
    val blockEnd = s"</$blockName>"
    UniversalScript(
      ShellScript(
        s"""filterBlock() {
           |  local reading=false
           |  while read -r
           |  do
           |    ${
          val `return` = Command("return")
          val printf = Command("printf")
          If('reading.eval,
            If('REPLY === blockEnd,
              `return`
            ).Else(Else(
              printf("%s\\n", 'REPLY)
            ))
          ).Else(If('REPLY === blockStart,
            'reading := True.command
          ))
        }
           |  done
           |}
           |
           |cat "$$0" | filterBlock | base64 -d '$outPath'""".stripMargin
      ),
      CmdScript(
        ""
      )
    )
  }

  def shellEscapeExec(path: String): String =
    path.replace("\\", "\\\\").replace(" ", "\\ ")

  def exec(path: String): UniversalScript = UniversalScript(
    ShellScript(
      s"""chmod +x '$path'
         |${shellEscapeExec(path)}""".stripMargin
    ),
    CmdScript(
      s""""$path""""
    )
  )

  def main(args: Array[String]): Unit = {
    println(unpackBlock("testblock", "/tmp/file1").string)
  }
}
