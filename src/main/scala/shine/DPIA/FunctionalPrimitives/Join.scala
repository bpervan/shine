package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.JoinAcc
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Join(n: Nat,
                      m: Nat,
                      w: AccessType,
                      dt: DataType,
                      array: Phrase[ExpType]) extends ExpPrimitive
{
  array :: expT(n`.`(m`.`dt), w)
  override val t: ExpType = expT({n * m}`.`dt, w)

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType],
                               Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]
                              ) : Phrase[AccType] = {
    import TranslationToImperative._
    val otype = C.t.inT.dataType
    fedAcc(env)(array)(λ(accT(otype))(o => JoinAcc(n, m, dt, C(o))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    import TranslationToImperative._
    acc(array)(JoinAcc(n, m, dt, A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    import TranslationToImperative._
    con(array)(λ(expT(n`.`(m`.`dt), read))(x => C(Join(n, m, w, dt, x)) ))
  }
}