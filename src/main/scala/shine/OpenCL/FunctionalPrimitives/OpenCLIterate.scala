package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.{acc, con}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.OpenCLIterateIAcc
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class OpenCLIterate(a: AddressSpace,
                               n: Nat,
                               m: Nat,
                               k: Nat,
                               dt: DataType,
                               f: Phrase[`(nat)->:`[ExpType ->: ExpType]],
                               array: Phrase[ExpType])
  extends ExpPrimitive
{
  f :: f.t.x ->: expT({f.t.x * n}`.`dt, read) ->: expT(f.t.x`.`dt, read)
  array :: expT({m * n.pow(k)}`.`dt, read)
  override val t: ExpType = expT(m`.`dt, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    con(array)(λ(expT({m * n.pow(k)}`.`dt, read))(x =>
      OpenCLIterateIAcc(a, n, m, k, dt, A,
        _Λ_[NatKind]()(l => λ(accT(l`.`dt))(o =>
          λ(expT({l * n}`.`dt, read))(x => acc(f(l)(x))(o)))),
        x) ))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = ???
}
