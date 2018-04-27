package de.lolhens.universalexe

import de.lolhens.universalexe.Shell.Value.{Raw, Var}

import scala.language.implicitConversions

object Shell {

  case class Indent(i: Int) extends AnyVal {
    def next: Indent = Indent(i + 1)
    def string: String = "  " * i
  }

  object Indent {
    val initial: Indent = Indent(0)
  }

  trait Value {

    import Value._

    def lines: Seq[String]

    def ===(value: Value): Call = test(this, Raw("="), value)

    def =!=(value: Value): Call = test(this, Raw("!="), value)

    def eval: Expr // = Command(lines)
  }

  object Value {
    private val test = Command("test")

    case class Const(const: String) extends Value {
      def line: String = s"'${const.replace("'", "'\"'\"'")}'"
      override def eval: Expr = Command(line)

      override def lines: Seq[String] = Seq(line)
    }

    implicit def stringConst(string: String): Const = Const(string)

    case class Var(name: String) extends Value {
      def line: String = s""""$$$name""""
      override def eval: Expr = Command(line)

      def :=(value: Value): Assign = Assign(this, value)

      override def lines: Seq[String] = Seq(line)
    }

    implicit def symbolVar(symbol: Symbol): Var = Var(symbol.name)

    case class Capture(expr: Expr) extends Value {
      override def lines: Seq[String] = Seq("\"$(") ++ expr.lines ++ Seq(")\"")

      override def eval: Expr = expr
    }

    def $(expr: Expr): Capture = Capture(expr)

    case class Raw(raw: String) extends Value {
      override def lines: Seq[String] = Seq(raw)

      override def eval: Expr = Command(raw)
    }

  }

  trait Sequence {
    def sequenceExpressions: Seq[MultilineExpr]
    def sequenceLines: Seq[String] = sequenceExpressions.flatMap(_.lines)

    def ++(sequence: Sequence): Sequence = new Sequence {
      override def sequenceExpressions: Seq[MultilineExpr] = Sequence.this.sequenceExpressions ++ sequence.sequenceExpressions
    }
  }

  object Sequence {
    def apply(exprs: MultilineExpr*): Sequence = new Sequence {
      override def sequenceExpressions: Seq[MultilineExpr] = exprs
    }

    def flatten(sequences: Sequence*): Sequence = Sequence(sequences.flatMap(_.sequenceExpressions): _*)
  }

  trait MultilineExpr extends Sequence {
    override def sequenceExpressions: Seq[MultilineExpr] = Seq(this)

    def lines: Seq[String]
  }

  case class Block(sequence: Sequence) extends MultilineExpr {
    override def lines: Seq[String] = sequence.sequenceLines.map(e => s"  $e")
  }

  trait Expr extends MultilineExpr

  case class SingleLine(sequence: Sequence) extends Expr {
    override def lines: Seq[String] = Seq(line)
    def line: String = sequence.sequenceLines.mkString("; ")
  }

  trait Call extends Expr {
    val command: Command
    val args: Seq[Value]

    def withCommand(newCommand: Command): Call
    def withArgs(newArgs: Seq[Value]): Call

    def apply(args: Value*): Call = withArgs(this.args ++ args)

    override def lines: Seq[String] = Seq(command.name + args.map(e => s" ${e.lines.mkString("\n")}").mkString)
  }

  class CallImpl(val command: Command,
                 val args: Seq[Value]) extends Call {
    override def withCommand(newCommand: Command): Call = new CallImpl(newCommand, args)
    override def withArgs(newArgs: Seq[Value]): Call = new CallImpl(command, newArgs)
  }

  case class Command(name: String) extends Call {
    override def lines: Seq[String] = Seq(name)

    override val command: Command = this
    override val args: Seq[Value] = Seq.empty

    override def withCommand(newCommand: Command): Command = this
    override def withArgs(newArgs: Seq[Value]): Call = new CallImpl(command, newArgs)
  }

  object Command {
    private def escapedCommand(command: String): String =
      command.replace("\\", "\\\\").replace(" ", "\\ ")

    def escape(name: String): Command = Command(escapedCommand(name))
  }

  val True = Command("true")

  val False = Command("false")

  case class Assign(variable: Var, value: Value) extends Expr {
    def line: String = s"${variable.name}=${value.lines.mkString("\n")}"
    override def lines: Seq[String] = Seq(line)
  }

  def local(assign: Assign): Assign = new Assign(assign.variable, assign.value) {
    override def line: String = s"local ${super.line}"
  }

  trait Construct extends MultilineExpr {
    override def lines: Seq[String] = sequence.sequenceLines

    def sequence: Sequence
  }

  case class While(expr: MultilineExpr)
                  (val body: Sequence) extends Construct {

    import While._

    def sequence: Sequence = Sequence(
      `while`(Raw(expr.lines.mkString("\n"))),
      `do`,
      Block(body),
      `done`
    )
  }

  object While {
    private val `while` = Command("while")
    private val `do` = Command("do")
    private val `done` = Command("done")
  }

  case class If(expr: MultilineExpr)
               (val body: Sequence,
                elseBranchOption: Option[Sequence] = None) extends Construct {

    import If._

    def sequence: Sequence = condition(`if`)

    def condition(command: Call): Sequence = Sequence(
      command(Raw(expr.lines.mkString("\n"))),
      Block(body),
    ) ++
      elseBranch

    def elseBranch: Sequence = elseBranchOption match {
      case Some(elifBranch: If) =>
        elifBranch.condition(`elif`)

      case Some(elseBranch) => Sequence(
        `else`,
        Block(elseBranch),
        `fi`
      )

      case None =>
        `fi`
    }

    def Else(elseBranch: MultilineExpr): If = If(expr)(body, Some(elseBranch))
  }

  object If {
    private val `if` = Command("if")
    private val `then` = Command("then")
    private val `elif` = Command("elif")
    private val `else` = Command("else")
    private val `fi` = Command("fi")
  }

}