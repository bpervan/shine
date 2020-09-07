package shine.OpenCL

import rise.core.DSL._
import rise.core.types._
import rise.openCL.DSL._
import util.gen

class ReduceByIndexSeq extends shine.test_util.Tests {

  private def xsT(N: NatIdentifier) = ArrayType(N, int)
  private def isT(N: NatIdentifier) = ArrayType(N, NatType)

  val add = fun(x => fun(a => x + a))

  test("Reduce By Index Seq Test") {

    val reduceByIndexSeqTest = nFun(n => nFun(k => fun(xsT(k))(hist => fun(isT(n))(is => fun(xsT(n))(xs =>
      reduceByIndexSeq(rise.core.types.AddressSpace.Global)(add)(hist)(is)(xs) |>
        mapSeq(fun(x => x))
    )))))

    gen.OpenCLKernel(reduceByIndexSeqTest)
  }

}
