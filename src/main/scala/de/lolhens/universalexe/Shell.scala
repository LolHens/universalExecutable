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
    def string(implicit ind: Indent): String

    override def toString: String = string(Indent.initial)
  }

  abstract class Value extends ShellScript {
    def ===(value: Value): Command = Command("test")(this, Raw("="), value)

    def =!=(value: Value): Command = Command("test")(this, Raw("!="), value)

    def eval: Expr
  }

  object Value {

    case class Const(const: String) extends Value {
      override def string(implicit ind: Indent): String = s"'${const.replace("'", "'\"'\"'")}'"
      override def eval: Expr = Raw(const).eval
    }

    implicit def stringConst(string: String): Const = Const(string)

    case class Var(name: String) extends Value {
      override def string(implicit ind: Indent): String = s""""$$$name""""

      def :=(value: Value): Assign = Assign(this, value)

      override def eval: Expr = Raw(s"$$$name").eval
    }

    implicit def symbolVar(symbol: Symbol): Var = Var(symbol.name)

    case class $(expr: Expr) extends Value {
      override def string(implicit ind: Indent): String = s""""$$(${expr.string})""""
      override def eval: Expr = expr
    }

    implicit def captureExpr(expr: Expr): $ = $(expr)

    case class Raw(raw: String) extends Value {
      override def string(implicit ind: Indent): String = raw
      override def eval: Expr = Command(raw)
    }
  }

  abstract class Script extends ShellScript {
    def expressions: Seq[Expr]
    override def string(implicit ind: Indent): String = ???
  }

  abstract class Expr extends Script

  abstract class ExprSeq extends Expr {
    def exprs: Seq[Expr]
    def string(implicit ind: Indent): String = exprs.map(e => ind.string + e.string(ind.next)).mkString("\n")
  }

  case class Imperative(exprs: Expr*)

  case class Command(command: String, args: Value*) extends Expr {
    override def string(implicit ind: Indent): String = command + args.map(e => s" ${e.string}").mkString

    def withCommand(newCommand: String): Command = Command(newCommand, args:_*)
    def withArgs(newArgs: Seq[Value]): Command = Command(command, newArgs:_*)

    def apply(args: Value*): Command = Command(command, this.args ++ args:_*)
  }

  val True = Command("true")

  val False = Command("false")

  case class Assign(variable: Var, value: Value) extends Expr {
    override def string(implicit ind: Indent): String = s"${variable.name}=${value.string}"
  }

  class Else(val body: Expr) extends Script {
    def elif: String = "else"
    def elseBranch(implicit ind: Indent): String = "fi"
    override def exprs: Seq[Expr] = Seq(body)
  }

  object Else {
    def apply(body: Expr): Else = new Else(body)
  }

  case class If(expr: Expr,
                override val body: Expr,
                elseBranchOption: Option[Else] = None) extends Else(body) {
    override def exprs: Seq[Expr] = Seq(
      Command("if", Raw(expr.string)),
      Command("then"),
      body,
      elseBranch
    )

    override def string(implicit ind: Indent): String = s"if ${expr.string}\nthen\n${body.string}\n$elseBranch"
    override def elif: String = "elif"
    override def elseBranch(implicit ind: Indent): String = elseBranchOption match {
      case Some(elseBranch) => s"${elseBranch.elif}\n${elseBranch.body.string}\n${elseBranch.elseBranch}"
      case None => "fi"
    }

    def Else(elseBranch: Else): If = If(expr, body, Some(elseBranch))
  }
}