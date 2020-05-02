package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Gather(n: Nat, m: Nat, dt: DataType,
                        indices: Phrase[ExpType],
                        input: Phrase[ExpType]) extends ExpPrimitive
{
  indices :: expT(m`.`idx(n), read)
  input :: expT(n`.`dt, read)
  override val t: ExpType = expT(m`.`dt, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    throw new Exception("this should not happen")

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import shine.DPIA.Compilation.TranslationToImperative._

    con(indices)(fun(expT(m`.`idx(n), read))(y =>
      con(input)(fun(expT(n`.`dt, read))(x =>
        C(Gather(n, m, dt, y, x))
      ))
    ))
  }
}
