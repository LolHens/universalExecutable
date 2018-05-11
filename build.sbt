name := "universal-executable"
version := "1.0.0"

scalaVersion := "2.12.6"

def universalScript(shellCommands: String,
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

def defaultUniversalScript(javaOpts: Seq[String] = Seq.empty,
                           shebang: Boolean = false): Seq[String] = {
  val javaOptsString = javaOpts.map(_ + " ").mkString
  Seq(universalScript(
    shellCommands = s"""exec java -jar $javaOptsString$$JAVA_OPTS "$$0" "$$@"""",
    cmdCommands = s"""java -jar $javaOptsString%JAVA_OPTS% "%~dpnx0" %*""",
    shebang = shebang
  ))
}

assemblyOption in assembly := (assemblyOption in assembly).value
  .copy(prependShellScript = Some(defaultUniversalScript(shebang = true)))

assemblyJarName in assembly := s"${name.value}-${version.value}.sh.bat"
