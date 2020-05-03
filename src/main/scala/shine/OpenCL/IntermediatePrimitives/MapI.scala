package shine.OpenCL.IntermediatePrimitives

import shine.DPIA.DSL.{comment, λ, _}
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.DataType.idx
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read}
import shine.DPIA.{->:, Nat, accT, expT}
import shine.OpenCL.DSL.barrier
import shine.OpenCL.ImperativePrimitives._
import shine.OpenCL._

final case class MapI(level: ParallelismLevel, dim: Int) {
  def apply(n: Nat, dt1: DataType, dt2: DataType,
            f: Phrase[ExpType ->: AccType ->: CommType],
            in: Phrase[ExpType],
            out: Phrase[AccType]): Phrase[CommType] =
  {
    level match {
      case Global =>
        comment("mapGlobal")`;`
        ParForGlobal(dim)(n, dt2, out,
          λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a))))
      case Local =>
        comment("mapLocal")`;`
        ParForLocal(dim)(n, dt2, out,
          λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a)))) `;`
        barrier()
      case WorkGroup =>
        comment("mapWorkgroup")`;`
        ParForWorkGroup(dim)(n, dt2, out,
          λ(expT(idx(n), read))(i => λ(accT(dt2))(a => f(in `@` i)(a))))
      case Sequential => ???
    }
  }
}
