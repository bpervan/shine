package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, NatData}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class AsIndex(n: Nat,
                         e: Phrase[ExpType]) extends ExpPrimitive
{
  e :: expT(NatType, read)
  override val t: ExpType = expT(idx(n), read)

  override def eval(s: OperationalSemantics.Store): OperationalSemantics.Data =
    OperationalSemantics.eval(s, e) match {
      case NatData(m) => IndexData(m, n)
      case d => throw new Exception(s"Expected NatData but found $d.")
    }

  def acceptorTranslation(A: Phrase[AccType])
                         (implicit context: TranslationContext
                         ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(fun(expT(NatType, read))(x =>
      A :=|IndexType(n)| AsIndex(n, x)))
  }

  def continuationTranslation(C: Phrase[ExpType ->: CommType])
                             (implicit context: TranslationContext
                             ): Phrase[CommType] = {
    import TranslationToImperative._

    con(e)(λ(e.t)(x =>
      C(AsIndex(n, x))))
  }
}
