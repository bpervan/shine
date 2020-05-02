package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.IntermediatePrimitives.ScanSeqI
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class ScanSeq(n: Nat,
                         dt1: DataType,
                         dt2: DataType,
                         f: Phrase[ExpType ->: ExpType ->: ExpType],
                         init:Phrase[ExpType],
                         array: Phrase[ExpType]) extends ExpPrimitive
{
  f :: expT(dt1, read) ->: expT(dt2, read) ->: expT(dt2, write)
  init :: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.con(array)(λ(expT(n`.`dt1, read))(x =>
      TranslationToImperative.con(init)(λ(expT(dt2, read))(y =>
        ScanSeqI(n, dt1, dt2,
          λ(expT(dt1, read))(x => λ(expT(dt2, read))(y => λ(accT(dt2))(o =>
            TranslationToImperative.acc(f(x)(y))(o)))),
          y, x, A)
      )
      ))
    )

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    `new`(n`.`dt2, λ(varT(n`.`dt2))(tmp =>
      TranslationToImperative.acc(this)(tmp.wr) `;` C(tmp.rd) ))
}
