package rise.core

import arithexpr.arithmetic.Cst
import rise.core.TypedDSL._
import rise.core.TypeLevelDSL._
import rise.core.types._
import rise.core.primitives._
import rise.core.semantics.NatData
import rise.openCL.primitives._
import shine.C.SizeInByte
import shine.OpenCL
import shine.OpenCL.{GlobalSize, HNilHelper, LocalSize, ScalaFunction, `(`, `)=>`, `,`}
import util.Execute

class dependentTypes extends test_util.TestsWithExecutor {
  test("Infer int addition type") {
    val e =
      depFun((n: Nat) =>
        fun(
          DepArrayType(n, n2dtFun(i => (i + 1) `.` f32)) ->: DepArrayType(
            n,
            n2dtFun(i => (i + 1) `.` f32)
          )
        )(xs => xs |> depMapSeq(depFun((i: Nat) => fun(xs => mapSeq(fun(x => x))(xs::((i+1)`.`f32)))))
      ))
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    println(inferred.t)
  }

  test("Dependent pair construct") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(xs => dpair(n)(mapSeq(fun(x => x))(xs)))
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("Dependent pair take and untake") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(xs =>
        dmatch(dpair(n)(xs))(
          depFun((_:Nat) => fun(xs => xs |> take(5) |> mapSeq(fun(x => x))))
        )
      )
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("GEN: Dependent pair map increment") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((n:Nat) => fun(xs =>
        dpair(n)(mapSeq(fun(x => x + l(1.0f)))(xs) ::(n`.`f32))
      ))))
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val cFunName = "foo"
    val cFun = util.gen.CProgram(inferred, cFunName)

    val testCode =
      s"""
         |#include<stdlib.h>
         |#include<stdio.h>
         |#include<stdint.h>
         |
         |${cFun.code}
         |
         |int main(int argc, char** argv) {
         |    const uint32_t x = 100;
         |
         |    const size_t data_size = sizeof(uint32_t) + x * sizeof(float);
         |    uint8_t data[data_size];
         |    uint8_t output[data_size];
         |    // Gold has same first, and incremented snd
         |    uint8_t gold[data_size];
         |
         |    ((uint32_t*)data)[0] = x;
         |    ((uint32_t*)gold)[0] = x;
         |
         |    float* floats = (float*)(data + sizeof(uint32_t));
         |    float* gold_floats = (float*)(gold + sizeof(uint32_t));
         |
         |    for (int i = 0; i < x; i++) {
         |        floats[i] = (float)i;
         |        gold_floats[i] = ((float)i) + 1.0;
         |    }
         |
         |    $cFunName(output, data);
         |    for (size_t i = 0; i < x; i++) {
         |        if (output[i] != gold[i]) {
         |            return 1;
         |        }
         |    }
         |    return 0;
         |}""".stripMargin
         Execute(testCode)
  }

  test("GEN: Dependent pair match with reduction") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((_: Nat) => fun(xs =>
        reduceSeq(fun(x => fun(y => x + y)))(l(0.0f))(xs))
      ))
    )
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val cFunName = "foo"
    val cFun = util.gen.CProgram(inferred, cFunName)

    val testCode =
      s"""
        |#include<stdlib.h>
        |#include<stdio.h>
        |#include<stdint.h>
        |
        | ${cFun.code}
        |
        |int main(int argc, char** argv) {
        |    const uint32_t x = 3;
        |
        |    uint8_t data[sizeof(uint32_t) + x*sizeof(float)];
        |
        |    ((uint32_t*)data)[0] = x;
        |
        |    float* floats = (float*)(data + sizeof(uint32_t));
        |
        |    float gold = 0.0;
        |    for (int i = 0; i < x; i++) {
        |        floats[i] = (float)i;
        |        // Solution is just sum
        |        gold += (float)i;
        |    }
        |    float output;
        |
        |    $cFunName(&output, data);
        |    printf("%f", output);
        |    if (output == gold) { return 0; } else { return 1; }
        |}
        |""".stripMargin

    Execute(testCode)
  }

  test("GENOCL: Dependent pair map increment") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((n:Nat) => fun(xs =>
        dpair(n)(mapSeq(fun(x => x + l(1.0f)))(xs) ::(n`.`f32))
      ))))
    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val cFunName = "foo"
    val cFun = util.gen.OpenCLKernel(inferred, cFunName)
  }


  test("Dependent pair match with taking") {
    val e = fun(Nat `**` (n => n`.`f32))(pair =>
      dmatch(pair)(depFun((_:Nat) => fun(xs => mapSeq(fun(x => x))(take(5)(xs)))))
    )
    val inferred: Expr = TDSL.inferDependent(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }


  test("Simple count") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => count(array)(fun(x => x =:= l(0.0f))))
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("Simple which") {
    val e = depFun((n: Nat) => depFun((count:Nat) =>
      fun(n `.` f32)(array => mapSeq(fun(x => x))(which(array)(count)(fun(x => x =:= l(0.0f)))))
    ))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("Filter and sum") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => {
        def pred = fun(x => x =/= l(0.0f))
        val cnt = count(array)(pred)
        liftN(indexAsNat(cnt))(depFun((cnt: Nat) =>
          reduceSeq(fun(x => fun(_ => x)))(lidx(0, n))(which(array)(cnt)(pred))
        ))
      })
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("List of list one row") {
    val e = depFun((n: Nat) =>
      fun(n `.` f32)(array => {
        def pred = fun(x => x =/= l(0.0f))
        val cnt = count(array)(pred)
        liftN(indexAsNat(cnt))(depFun((cnt: Nat) =>
           dpair(cnt)(mapSeq(fun(idx => pair(array `@` idx)(idx)))(which(array)(cnt)(pred)))
        ))
      })
    )

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("List of list") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))
      def cnts = toMem(mapSeq(fun(row => map(pred)(row) |> count |> indexAsNat))(array))

      liftNats(cnts)(depFun((lengths:NatCollection) =>
        dpairNats(lengths)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            which(map(pred)(row))(lengths `@` rowIdx)
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }


  test("Compressed sparse row") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))


      def offs = toMem(array |>
        map(fun(row => map(pred)(row) |> count |> indexAsNat))
        |> scanSeq(fun(x => fun(y => x + y)))(Literal(NatData(0)))
      )

      liftNats(offs)(depFun((offs:NatCollection) =>
        dpairNats(offs)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            which(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.CProgram(inferred, "Foo_foo")
  }

  test("List of list OCL") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))
      def cnts = toMem(mapSeq(fun(row => map(pred)(row) |> oclCount(AddressSpace.Global) |> indexAsNat))(array))

      liftNats(cnts)(depFun((lengths:NatCollection) =>
        dpairNats(lengths)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            oclWhich(map(pred)(row))(lengths `@` rowIdx)
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.OpenCLKernel(inferred, "Foo_foo")
  }

  test("Compressed sparse row OCL") {
    val e = depFun((n: Nat) => depFun((m: Nat)=> fun(n `.` m `.` f32)(array => {
      def pred = fun(x => x =/= l(0.0f))


      def offs = oclToMem(AddressSpace.Global)(array |>
        map(fun(row => map(pred)(row) |> oclCount(AddressSpace.Global) |> indexAsNat))
        |> oclScanSeq(AddressSpace.Global)(fun(x => fun(y => x + y)))(Literal(NatData(0)))
      )

      liftNats(offs)(depFun((offs:NatCollection) =>
        dpairNats(offs)(toDepArray(array) |>
          depMapSeq(depFun((rowIdx:Nat) => fun(row =>
            oclWhich(map(pred)(row))((offs `@` (rowIdx + 1)) - (offs `@` rowIdx))
              |> mapSeq(fun(nnzIdx => pair(nnzIdx)(row `@` nnzIdx)))
          ))))
      ))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    util.gen.OpenCLKernel(inferred, "Foo_foo")
  }

  test("C filter even numbers") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(pred) |> count |> indexAsNat)(depFun((count:Nat) =>
        dpair(count)(which(array |> map(pred))(count)
          |> mapSeq(fun(idx => array `@` idx)))
      ))
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val program = util.gen.CProgram(inferred, "kernel")

    println(program.code)

    val code = raw"""
      |
      |#include<stdlib.h>
      |#include<stdio.h>
      |#include<stdint.h>
      |
      |${program.code}

      |
      |int main(int argc, char** argv) {
      |   const uint32_t input_size = 10000;
      |
      |   int input[input_size];
      |
      |   for (int i = 0; i < input_size; i++) {
      |       input[i] = i;
      |   }
      |
      |   int output[1 + input_size/2];
      |
      |   kernel((uint8_t*)output, input_size, input);
      |
      |   int ret = 0;
      |   for(uint32_t i = 0; i < input_size/2; i++) {
      |       uint32_t value = output[1 + i];
      |       if (value != i*2) {
      |         printf("!!!!!{%d, %d}", i, value);
      |         return 1;
      |       }
      |       printf("(%d,%d)", i, value);
      |
      |   }
      |   return 0;
      |}""".stripMargin

    Execute(code)
  }

  test("OCL count numbers") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Global)
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernel = util.gen.OpenCLKernel(inferred, "count")

    // Testing it
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val n = 1000
    val array = Array.tabulate(n)(i => i)
    val (result, _) = kernelF(n `,` array)
    assert(result(0) == n / 2)
  }

  test("OCL count numbers parallel") {
    val e = depFun((n: Nat) => fun(n `.` int)(array => {
      array |> split(8) |> mapGlobal(0)(
        map(fun(x => (x % l(2)) =:= l(0))) >> split(8) >> mapSeqUnroll(oclCount(AddressSpace.Private))
      )
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)
    val kernel = util.gen.OpenCLKernel(inferred, "ocl_par_count")

    // Testing it
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))
    val power = 10
    val n = Math.pow(2, power).toInt
    val array = Array.tabulate(n)(i => i)
    val (result, _) = kernelF(n `,` array)
    val total = result.sum
    assert(total == n / 2)
  }

  test("OCL filter even numbers with outside count") {
    val e = depFun((n: Nat) => depFun((count:Nat) =>  fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      oclWhich(array |> map(pred))(count) |> oclToMem(AddressSpace.Global) |> mapSeq(fun(idx => array `@` idx))
    })))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    val kernel = util.gen.OpenCLKernel(inferred, "ocl_filter")

    val n = 1000
    val array = Array.tabulate(n)(i => i)
    val even = array.filter(_ % 2 == 0)
    val kernelF = kernel.as[ScalaFunction`(`Int`,`Int `,` Array[Int]`)=>`Array[Int]].withSizes(LocalSize(1), GlobalSize(1))

    val (oclEven, _) = kernelF(n `,` even.length `,` array)
    assert(even.length == oclEven.length)
    assert(even.zip(oclEven).forall( { case (x, y) => x == y} ))
  }

  test("OCL filter even number bringing it with me") {
    val e = depFun((n: Nat) =>  fun(n `.` int)(array => {
      def pred = fun(x => (x % l(2)) =:= l(0))
      liftN(array |> map(fun(x => (x % l(2)) =:= l(0))) |> oclCount(AddressSpace.Private) |> indexAsNat)(
        depFun((count:Nat) => dpair(count)(array |> mapSeq(fun(x => x)))
      ))
    }))

    val inferred: Expr = TDSL.infer(e)
    println(inferred)
    print(inferred.t)

    {
      val kernelWrap = util.gen.OpenCLKernel(inferred, "ocl_filter")

      val kernel = kernelWrap.copy(kernel = kernelWrap.kernel.withFallbackOutputSize(SizeInByte(1024 * 1024)))
      val n = 1000
      val array = Array.tabulate(n)(i => i)
      val even = array.filter(_ % 2 == 0)
      val kernelF = kernel.as[ScalaFunction `(` Int `,` Array[Int] `)=>` (Int, Array[Int])].withSizes(LocalSize(1), GlobalSize(1))

      val (dpair, _) = kernelF(n `,` array)
      val (count, data) = dpair
    }
  }
}
