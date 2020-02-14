package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA.Types.DataType._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Array(dt: DataType,
                       elements: Vector[Phrase[ExpType]])
  extends ExpPrimitive {

  override val t: ExpType = expT({elements.length : Nat}`.`dt, read)

  override def prettyPrint: String =
    s"${this.getClass.getSimpleName} (${elements.flatMap(PrettyPhrasePrinter(_))})"

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext): Phrase[CommType] = {
    ???
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._

    def rec(func: Vector[Phrase[ExpType]], imp: Vector[Phrase[ExpType]]): Phrase[CommType] = {
      func match {
        case xf +: func => con(xf)(fun(expT(dt, read))(xi =>
          rec(func, imp :+ xi)
        ))
        case _ => C(Array(dt, imp))
      }
    }

    rec(elements, Vector())
  }
}
