package de.lolhens.universalexe

import de.lolhens.universalexe.CaseClass.CopierFactory

trait CaseClass extends Product {
  def copier[T >: this.type <: CaseClass](implicit factory: CopierFactory[T]): factory.Copier =
    factory.copier(this)

  override def productPrefix: String

  def products: List[Any] = Nil

  override def productElement(n: Int): Any = products(n)
  override def productArity: Int = products.size
  override def productIterator: Iterator[Any] = products.iterator

  override def canEqual(that: Any): Boolean = getClass.isInstance(that)

  override def equals(other: Any): Boolean = other match {
    case caseClass: CaseClass if getClass.isInstance(caseClass) && caseClass.canEqual(this) =>
      caseClass.productIterator.zip(productIterator).forall {
        case (a, b) => a == b
      }
    case _ => false
  }

  override def hashCode(): Int = productIterator.foldLeft(0) { (sum, field) =>
    31 * sum + field.hashCode()
  }

  override def toString: String = productIterator.mkString(productPrefix + "(", ",", ")")
}

object CaseClass {

  abstract class CopierFactory[T <: CaseClass] {
    type Copier
    def copier(caseClass: T): Copier
  }

  object CopierFactory {
    type Aux[T <: CaseClass, C] = CopierFactory[T] {type Copier = C}

    def apply[T <: CaseClass, C](c: T => C): CopierFactory.Aux[T, C] = new CopierFactory[T] {
      type Copier = C
      def copier(caseClass: T): Copier = c(caseClass)
    }
  }

}
