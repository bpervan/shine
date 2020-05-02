package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class PrintType(msg: String,
                           dt: DataType,
                           input: Phrase[ExpType]) extends ExpPrimitive
{
  println(s"$msg : $dt (DPIA level)")

  input :: expT(dt, read)
  override val t: ExpType = expT(dt, read)

  override def eval(s: Store): Data = OperationalSemantics.eval(s, input)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.acc(input)(A)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(input)(C)
}
