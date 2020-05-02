package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Let(dt1: DataType, dt2: DataType,
                     value: Phrase[ExpType],
                     f: Phrase[ExpType ->: ExpType]) extends ExpPrimitive
{
  value :: expT(dt1, read)
  f :: expT(dt1, read) ->: expT(dt2, read)
  override val t: ExpType = expT(dt2, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    con(value)(fun(value.t)(x => acc(f(x))(A)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._
    con(value)(fun(value.t)(x => con(f(x))(C)))
  }
}
