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

  trait ShellScript {
    def string: String

    override def toString: String = string
  }

  abstract class Value extends ShellScript {
    def ===(value: Value): Command = Command("test")(this, Raw("="), value)

    def =!=(value: Value): Command = Command("test")(this, Raw("!="), value)

    def eval: Expr = Command(string)
  }

  object Value {

    case class Const(const: String) extends Value {
      override def string: String = s"'${const.replace("'", "'\"'\"'")}'"
    }

    implicit def stringConst(string: String): Const = Const(string)

    case class Var(name: String) extends Value {
      override def string: String = s""""$$$name""""

      def :=(value: Value): Assign = Assign(this, value)
    }

    implicit def symbolVar(symbol: Symbol): Var = Var(symbol.name)

    case class Capture(expr: Expr) extends Value {
      override def string: String = s""""$$(${expr.string})""""
      override def eval: Expr = expr
    }

    def $(expr: Expr): Capture = Capture(expr)

    case class Raw(raw: String) extends Value {
      override def string: String = raw
    }
  }

  abstract class Sequence {
    def expressions: Seq[MultilineExpr]

    def indentedString(indentation: Int): String = expressions.map(e => "  " * indentation + e.string).mkString("\n")
  }

  object Sequence {
    def apply(sequences: Sequence*): Sequence = new Sequence {
      override def expressions: Seq[MultilineExpr] = sequences.flatMap(_.expressions)
    }
  }

  case class Block(sequence: Sequence) extends MultilineExpr {
    override def expressions: Seq[MultilineExpr] = sequence.expressions

    override def indentedString(indentation: Int): String = super.indentedString(indentation + 1)
  }

  abstract class MultilineExpr extends Sequence {
    override def expressions: Seq[MultilineExpr] = Seq(this)

    def string: String = indentedString(0)
  }

  abstract class Expr extends MultilineExpr {
    override def expressions: Seq[Expr] = Seq(this)
  }

  //case class OnSingleLine(multilineExpr: MultilineExpr) extends Expr

  case class Command(command: String, args: Value*) extends Expr {
    override def string: String = escapedCommand + args.map(e => s" ${e.string}").mkString

    def escapedCommand: String = command.replace("\\", "\\\\").replace(" ", "\\ ")

    def withCommand(newCommand: String): Command = Command(newCommand, args:_*)
    def withArgs(newArgs: Seq[Value]): Command = Command(command, newArgs:_*)

    def apply(args: Value*): Command = Command(command, this.args ++ args:_*)
  }

  val True = Command("true")

  val False = Command("false")

  case class Assign(variable: Var, value: Value) extends Expr {
    override def string: String = s"${variable.name}=${value.string}"
  }

  case class If(expr: MultilineExpr)
               (val body: Sequence,
                elseBranchOption: Option[MultilineExpr] = None) extends MultilineExpr {
    import If._

    override def indentedString(indentation: Int): String = sequence().indentedString(indentation)

    def sequence(command: Command = `if`): Sequence = Sequence(
      command(Raw(expr.string)),
      Block(body),
      elseBranch
    )

    def elseBranch: Sequence = elseBranchOption match {
      case Some(elifBranch: If) =>
        elifBranch.sequence(`elif`)

      case Some(elseBranch) => Sequence(
        `else`,
        elseBranch,
        `fi`
      )

      case None =>
        `fi`
    }

    def Else(elseBranch: MultilineExpr): If = If(expr)(body, Some(elseBranch))
  }

  object If {
    val `if` = Command("if")
    val `then` = Command("then")
    val `elif` = Command("elif")
    val `else` = Command("else")
    val `fi` = Command("fi")
  }
}