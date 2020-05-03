package shine.C

import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.Assign
import shine.DPIA.IntermediatePrimitives.DepMapSeqI
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types._

class TranslationContext() extends shine.DPIA.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case _: ScalarType => Assign(dt, lhs, rhs)

      case _: IndexType => Assign(dt, lhs, rhs)

      //TODO think about this more thoroughly
      case PairType(_, _) => Assign(dt, lhs, rhs)
        // assign(dt1, recordAcc1(dt1, dt2, lhs), fst(rhs)) `;` assign(dt2, recordAcc2(dt1, dt2, lhs), snd(rhs))

      //TODO makes a decision. Not allowed!
      case DepArrayType(n, ft) =>
        DepMapSeqI(unroll = false)(n, ft, ft,
          depFun[NatKind]()(k =>
            λ(ExpType(ft(k), read))(x => λ(AccType( ft(k) ))(a => assign(ft(k), a, x) ))),
          rhs, lhs)

      case x => throw new Exception(s"Don't know how to assign value of type $x")
    }
  }
}
