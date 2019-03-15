package idealised.DPIA.Primitives

import idealised.SurfaceLanguage.DSL._
import idealised.SurfaceLanguage.NatIdentifier
import idealised.SurfaceLanguage.Types._
import idealised.util.SyntaxChecker
import lift.arithmetic._

class Split extends idealised.util.Tests {

  test("Simple split example should generate syntactic valid C code with two for loops") {
    val slideExample =
      nFun(n =>
        fun(ArrayType(n, float))(xs => xs :>> split(2) :>> mapSeq(mapSeq(fun(x => x)))))

    val p = idealised.C.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 2
  }

  test("Simple 2D split example with separate maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))(xs =>
          xs :>> map(split(2)) :>> mapSeq(mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }

  test("Simple 2D split example with merged maps should generate syntactic valid OpenMP code with three for loops") {
    val slideExample =
      nFun(n => nFun(m =>
        fun(ArrayType(n, ArrayType(m, float)))( xs =>
          xs :>> mapSeq(split(2) >>> mapSeq(mapSeq(fun(x => x)))) )))

    val p = idealised.OpenMP.ProgramGenerator.makeCode(TypeInference(slideExample, Map()).toPhrase)
    val code = p.code
    SyntaxChecker(code)
    println(code)

    "for".r.findAllIn(code).length shouldBe 3
  }


  ignore("Split mapAcc translation could work with functions working on independent elements") {
    val e = nFun(n => fun(ArrayType(n, float))(xs =>
      xs :>> mapSeq(fun(x => x)) :>> split(8) :>> map(fun(x => x))
    ))

    val p = idealised.OpenCL.KernelGenerator.makeCode(TypeInference(e, Map()).toPhrase)
    val code = p.code
    SyntaxChecker.checkOpenCL(code)
    println(code)
  }
}