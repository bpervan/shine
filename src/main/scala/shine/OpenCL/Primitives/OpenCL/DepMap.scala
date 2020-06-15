package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.{acc, con}
import shine.DPIA.DSL.{_Λ_, λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types.{AccType, CommType, ExpType, NatKind, NatToData, read, write, _}
import shine.DPIA.{->:, Nat, NatIdentifier, `(nat)->:`, accT, expT, varT, _}
import shine.OpenCL.ParallelismLevel
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class DepMap(level: ParallelismLevel,
                        dim: Int)
                       (val n: Nat,
                        val ft1:NatToData,
                        val ft2:NatToData,
                        val f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                        val array: Phrase[ExpType]) extends ExpPrimitive
{
  f :: f.t.x ->: expT(ft1(f.t.x), read) ->: expT(ft2(f.t.x), write)
  array :: expT(n `.d` ft1, read)
  override val t: ExpType = expT(n`.d`ft2, write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    con(array)(λ(expT(n`.d`ft1, read))(x =>
      DepMapI(level, dim)(n, ft1, ft2, _Λ_[NatKind]()((k: NatIdentifier) =>
        λ(expT(ft1(k), read))(x => λ(accT(ft2(k)))(o => {
          acc(f(k)(x))(o)
        }))), x, A)))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    `new`(n`.d`ft2, λ(varT(n`.d`ft2))(tmp =>
      acc(this)(tmp.wr) `;` C(tmp.rd) ))
}
