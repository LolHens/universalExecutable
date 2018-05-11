package de.lolhens.universalexe

import de.lolhens.universalexe.Shell2.Value.Var

object Shell2 {
  trait Exprs {
    def exprs: Seq[Expr]

    def withExprs(newExprs: Seq[Expr]): Block = new Block {
      override def exprs: Seq[Expr] = newExprs
    }

    def ++(block: Block): Block = withExprs(exprs ++ block.exprs)

    def script(singleLine: Boolean = false): String = exprs.map(_.script).mkString(if (singleLine) "; " else "\n")
  }

  trait Block extends Exprs {

  }

  trait Expr extends Block {
    final override def exprs: Seq[Expr] = Seq(this)

    override final def script(singleLine: Boolean): String = script

    override final def withExprs(newExprs: Seq[Expr]): Block = new Block {
      override def exprs: Seq[Expr] = newExprs
    }

    def script: String
  }

  trait Value {
    def script: String
  }

  object Value {

    trait Const {
      def value: String
    }

    trait Var {
      def name: String
    }

    trait Capture {
      def block: Block
    }

    trait RawBlock {
      def block: Block
    }

  }


  trait Assign extends Expr {
    def variable: Var
    def value: Value

    override def script: String = s"${variable.name}=${value.script}"
  }

  trait Call extends Expr {
    def command: Command
    def args: Seq[Value]

    def withCommand(newCommand: Command): Call
    def withArgs(newArgs: Seq[Value]): Call

    def apply(args: Value*): Call = withArgs(this.args ++ args)

    override def script: String = command.name + args.map(e => s" ${e.script}").mkString
  }

  trait Command extends Call {
    def name: String

    override final def command: Command = this
    override final def args: Seq[Value] = Seq.empty

    override def withCommand(newCommand: Command): Call = newCommand
  }
}
