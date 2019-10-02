package idealised.DPIA.Primitives

import lift.core.DSL._
import lift.core.types._
import lift.core.primitives._
import lift.OpenCL.primitives._
import util.gen

class Let extends test_util.Tests {
  val id = fun(x => x)

  // TODO: it feels like toMem and let are closely related, should be merged?
  test("let private value") {
    def plusNum(n: Int, code: String): Unit = {
      "\\+".r.findAllIn(code).length shouldBe n
    }
    plusNum(2, gen.OpenCLKernel(fun(int)(x =>
      toPrivate(x + l(2)) |> fun(y => y * y)
    )).code)
    plusNum(2, gen.OpenCLKernel(fun(int)(x =>
      (x + l(2)) |> let(fun(y => y * y))
    )).code)
    plusNum(1, gen.OpenCLKernel(fun(int)(x =>
      toPrivate(x + l(2)) |> let(fun(y => y * y))
    )).code)
  }
}
