package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.AsScalarAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class AsScalar(n: Nat,
                          m: Nat,
                          dt: ScalarType,
                          array: Phrase[ExpType])
  extends ExpPrimitive
{
  array :: expT(n`.`vec(m, dt), read)
  override val t: ExpType = expT({n * m}`.`dt, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    import TranslationToImperative._
    acc(array)(AsScalarAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(array.t)(x => C(AsScalar(n, m, dt, x)) ))
  }

  override def fedeTranslation(env: Predef.Map[Identifier[ExpType],
    Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]
                              ): Phrase[AccType] = {
    import TranslationToImperative._
    fedAcc(env)(array)(fun(accT(C.t.inT.dataType))(o =>
      AsScalarAcc(n, m, dt, C(o))))
  }
}
