package shine.cuda

import shine.DPIA.FunctionalPrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.FloatData
import shine.DPIA.Types.MatrixLayout._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.FunctionalPrimitives.{OclToMem, OpenCLReduceSeq}
import shine.OpenCL._
import shine.cuda.primitives.functional._
import test_util.similar

class MMTest extends test_util.TestsWithYACX {
  val n = NatIdentifier(freshName("n"))
  val m = NatIdentifier(freshName("m"))
  val k = NatIdentifier(freshName("k"))

  val compilerOptions = scala.Array("--gpu-architecture=compute_70",
//    TODO only for run on palma
    "--include-path=/Applic.HPC/Easybuild/skylake/2019a/software/CUDA/10.1.105-GCC-8.2.0-2.31.1/targets/x86_64-linux/include",
    //doris
    "--include-path=/opt/cuda/targets/x86_64-linux/include")


  //Simple 16x16 matrix multiplication
  test("mmaTest for 16x16 matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixATile = Identifier(freshName("MatrixATile"), ExpType(ArrayType(mTile, ArrayType(kTile, f16)), read))
    val matrixBTile = Identifier(freshName("MatrixBTile"), ExpType(ArrayType(kTile, ArrayType(nTile, f16)), read))

    //Kernel
    val simpleMatMulTile =
      Lambda[ExpType, FunType[ExpType, ExpType]](matrixATile,
        Lambda[ExpType, ExpType](matrixBTile,
          //Write Result in output
          FromFragment(mTile, nTile, kTile, f32,

            //do matrix multiplication
            OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16),
              TensorMatMultAdd(mTile, nTile, kTile, Row_Major, Row_Major, f16, f32,

                //load aMatrix into a fragment
                OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.AMatrix, Row_Major),
                  ToFragment(mTile, nTile, kTile, f16, FragmentType.AMatrix,
                    matrixATile)),

                //load bMatrix into a fragment
                OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.BMatrix, Row_Major),
                  ToFragment(mTile, nTile, kTile, f16, FragmentType.BMatrix,
                    matrixBTile)),

                //add fragment with zeros
                OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16),
                  GenerateFragment(mTile, nTile, kTile, f32, Literal(FloatData(0.0f)), FragmentType.Acuumulator, Row_Major))))))
      )

    val kernel = shine.cuda.KernelGenerator.apply().makeCode(simpleMatMulTile, "matrixMult")

    println("KernelCode:")
    println(kernel.code)

    //Check kernel-result
    compilerOptions.foreach(kernel.kernel.addCompilerOption)

    val matrixATest = generateMatrix(mTile, kTile)
    val matrixBTest = generateMatrix(kTile, nTile)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    val scalaFun = kernel.as[ScalaFunction `(` scala.Array[scala.Array[Float]] `,`
      scala.Array[scala.Array[Float]] `)=>` scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

    val (result, _) = scalaFun(matrixATest `,` matrixBTest)

    val resultMatrix = result.sliding(mTile, nTile).toArray

    //Check result
    if (!similar(resultTest, resultMatrix)) {
      println("Expected:")
      print(resultTest.map(_.mkString(" ")).mkString("\n"))
      println()
      println("\nFound:")
      print(resultMatrix.map(_.mkString(" ")).mkString("\n"))
      println()
      throw new Exception("False Result")
    }
  }

  //Simple 16xk * kx16 matrix multiplication
  test("mmaTest for 16xk * kx16 matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixATile = Identifier(freshName("MatrixATile"), ExpType(ArrayType(mTile, ArrayType(k, f16)), read))
    val matrixBTile = Identifier(freshName("MatrixBTile"), ExpType(ArrayType(k, ArrayType(nTile, f16)), read))
    val matrixCFrag = Identifier(freshName("MatrixCFrag"), ExpType(Fragment(mTile, nTile, kTile, f32), read))
    val matrixABTiles = Identifier(freshName("MatrixABTiles"), ExpType(PairType(
      ArrayType(mTile, ArrayType(kTile, f16)),
      ArrayType(kTile, ArrayType(nTile, f16))), read))

    //Kernel
    val simpleMatMulTile =
      DepLambda[NatKind](k)(
        Lambda[ExpType, FunType[ExpType, ExpType]](matrixATile,
          Lambda[ExpType, ExpType](matrixBTile,
            FromFragment(mTile, nTile, kTile, f32,
              OpenCLReduceSeq(k /^ kTile, shine.cuda.AddressSpace.Private,
                PairType(
                  ArrayType(mTile, ArrayType(kTile, f16)),
                  ArrayType(kTile, ArrayType(nTile, f16))),
                Fragment(mTile, nTile, kTile, f32),

                Lambda[ExpType, FunType[ExpType, ExpType]](matrixCFrag,
                  Lambda[ExpType, ExpType](matrixABTiles,
                    TensorMatMultAdd(mTile, nTile, kTile, Row_Major, Row_Major, f16, f32,
                      OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.AMatrix, Row_Major),
                        ToFragment(mTile, nTile, kTile, f16, FragmentType.AMatrix,
                          Transpose(kTile, mTile, f16, read,
                            Fst(
                              ArrayType(kTile, ArrayType(mTile, f16)),
                              ArrayType(kTile, ArrayType(nTile, f16)),
                              matrixABTiles)))),

                      OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.BMatrix, Row_Major),
                        ToFragment(mTile, nTile, kTile, f16, FragmentType.BMatrix,
                          Snd(
                            ArrayType(mTile, ArrayType(kTile, f16)),
                            ArrayType(kTile, ArrayType(nTile, f16)),
                            matrixABTiles))),

                      matrixCFrag))),

                  GenerateFragment(mTile, nTile, kTile, f32, Literal(FloatData(0.0f)), FragmentType.Acuumulator, Row_Major),

                Zip(k /^ kTile,
                  ArrayType(mTile, ArrayType(kTile, f16)),
                  ArrayType(kTile, ArrayType(nTile, f16)),
                  read,

                  Split(kTile, k /^ kTile, read, ArrayType(mTile, f16),
                    Transpose(mTile, k, f16, read, matrixATile)),

                  Split(kTile, k /^ kTile, read, ArrayType(nTile, f16),
                    matrixBTile)
                ),

                false)))))

    val kernel = shine.cuda.KernelGenerator.apply().makeCode(simpleMatMulTile, "matrixMult")

    println("KernelCode:")
    println(kernel.code)


    //Check kernel-result
    compilerOptions.foreach(kernel.kernel.addCompilerOption)

    val kTest = 16 * 2

    val matrixATest = generateMatrix(mTile, kTest)
    val matrixBTest = generateMatrix(kTest, nTile)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    val scalaFun = kernel.as[ScalaFunction `(` Int `,` scala.Array[scala.Array[Float]] `,`
      scala.Array[scala.Array[Float]] `)=>` scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

    val (result, _) = scalaFun(kTest `,` matrixATest `,` matrixBTest)

    val resultMatrix = result.sliding(mTile, nTile).toArray

    //Check result
    if (!similar(resultTest, resultMatrix)) {
      println("Expected:")
      print(resultTest.map(_.mkString(" ")).mkString("\n"))
      println()
      println("\nFound:")
      print(resultMatrix.map(_.mkString(" ")).mkString("\n"))
      println()
      throw new Exception("False Result")
    }
  }


  //  matrixmultiplication of a mxk and a kx16 matrix
  test("mmaTest for mxk * kxn matrices produces expected result") {
    val mTile = 16
    val nTile = 16
    val kTile = 16

    val matrixA = Identifier(freshName("MatrixA"), ExpType(ArrayType(m, ArrayType(k, f16)), read))
    val matrixB = Identifier(freshName("MatrixBColumn"), ExpType(ArrayType(k, ArrayType(n, f16)), read))

    val matrixARow = Identifier(freshName("MatrixARow"), ExpType(ArrayType(mTile, ArrayType(k, f16)), read))
    val matrixBColumnT = Identifier(freshName("MatrixBColumn"), ExpType(ArrayType(nTile, ArrayType(k, f16)), read))

    val matrixCFrag = Identifier(freshName("MatrixCFrag"), ExpType(Fragment(mTile, nTile, kTile, f32), read))

    val matrixABTiles = Identifier(freshName("MatrixABTiles"), ExpType(PairType(
      ArrayType(kTile, ArrayType(mTile, f16)),
      ArrayType(kTile, ArrayType(nTile, f16))), read))

    //Kernel
    val simpleMatMul =
      DepLambda[NatKind](m)(
        DepLambda[NatKind](n)(
          DepLambda[NatKind](k)(
            //Input: matrixA
            Lambda[ExpType, FunType[ExpType, ExpType]](matrixA,
              //And matrixB
              Lambda[ExpType, ExpType](matrixB,
                Transpose(n, m, f32, read,
                  Join(n /^ nTile, nTile, read, ArrayType(m, f32),
                  //Map over nTile-column-block of matrixB
                  MapThreads('y')(n /^ nTile,
                    //A transposed column of matrixB
                    ArrayType(nTile, ArrayType(k, f16)),

                    //Result: transposed tile of cMatrix
                    ArrayType(nTile, ArrayType(m, f32)),

                    Lambda[ExpType, ExpType](matrixBColumnT,
                      //Transpose cTile
                      Transpose(m, nTile, f32, write,
                        Join(m /^ mTile, mTile, write, ArrayType(nTile, f32),
                          //Map over mTile-row-blocks of matrixA
                          MapWarp('x')(m /^ mTile,
                            //A row of matrixA
                            ArrayType(mTile, ArrayType(k, f16)),

                            //Result: tile of cMatrix
                            ArrayType(mTile, ArrayType(nTile, f32)),

                            Lambda[ExpType, ExpType](matrixARow,
                              FromFragment(mTile, nTile, kTile, f32,
                                //Multiply mTile rows of matrixA with kTest columns of matrixB
                                OpenCLReduceSeq(k /^ kTile, shine.cuda.AddressSpace.Private,
                                  //Input: Pair of transposed matrixATile and matrixBTile
                                  PairType(
                                    ArrayType(mTile, ArrayType(kTile, f16)),
                                    ArrayType(kTile, ArrayType(nTile, f16))),

                                  //Result: tile of cMatrix as fragment
                                  Fragment(mTile, nTile, kTile, f32),

                                  //Multiply matrixATile and matrixBTile
                                  Lambda[ExpType, FunType[ExpType, ExpType]](matrixCFrag,
                                    Lambda[ExpType, ExpType](matrixABTiles,
                                      //matrix multiply and accumulate
                                      TensorMatMultAdd(mTile, nTile, kTile, Row_Major, Row_Major, f16, f32,

                                        //matrixATile as fragment
                                        OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.AMatrix, Row_Major),
                                          ToFragment(mTile, nTile, kTile, f16, FragmentType.AMatrix,
                                            Transpose(kTile, mTile, f16, read,
                                              Fst(
                                                ArrayType(mTile, ArrayType(kTile, f16)),
                                                ArrayType(kTile, ArrayType(nTile, f16)),
                                                matrixABTiles)))),

                                        //matrixBTile as fragment
                                        OclToMem(shine.cuda.AddressSpace.Private, Fragment(mTile, nTile, kTile, f16, FragmentType.BMatrix, Row_Major),
                                          ToFragment(mTile, nTile, kTile, f16, FragmentType.BMatrix,
                                            Snd(
                                              ArrayType(mTile, ArrayType(kTile, f16)),
                                              ArrayType(kTile, ArrayType(nTile, f16)),
                                              matrixABTiles))),

                                        matrixCFrag))),

                                  //Neutral Element for Reduce: fragment initialized with zeros
                                  GenerateFragment(mTile, nTile, kTile, f32, Literal(FloatData(0.0f)), FragmentType.Acuumulator, Row_Major),

                                  //Zip transposed, splited row of matrixA and splited column of matrixB
                                  Zip(k /^ kTile,
                                    ArrayType(kTile, ArrayType(mTile, f16)),
                                    ArrayType(kTile, ArrayType(nTile, f16)),
                                    read,

                                    //Split transposed row of matrixA kTest.mTile.f16-Array -> k/kTest.kTest.mTile.f16-Array
                                    Split(kTile, k /^ kTile, read, ArrayType(mTile, f16),
                                      //Transpose row of matrixA mTile.kTest.f16-Array -> kTest.mTile.f16-Array
                                      Transpose(mTile, k, f16, read, matrixARow)),

                                    //Split transposed column of matrixB kTest.nTest.f16-Array -> k/kTest.kTest.nTest.f16-Array
                                    Split(kTile, k /^ kTile, read, ArrayType(nTile, f16),
                                      Transpose(nTile, k, f16, read, matrixBColumnT))
                                  ),

                                  false))),

                            //Split aMatrix in mTile-row-blocks
                            Split(mTile, m /^ mTile, read, ArrayType(k, f16), matrixA))
                        ))),

                    //Split bMatrix in nTile-column-blocks
                    Split(nTile, n /^ nTile, read, ArrayType(k, f16),
                      Transpose(k, n, f16, read, matrixB))))
              ))))))

    val kernel = shine.cuda.KernelGenerator.apply().makeCode(simpleMatMul, "matrixMult")

    println("KernelCode:")
    println(kernel.code)


    //Check kernel-result
    compilerOptions.foreach(kernel.kernel.addCompilerOption)

    val mTest = 32
    val nTest = 32
    val kTest = 32

    val matrixATest = generateMatrix(mTest, kTest)
    val matrixBTest = generateMatrix(kTest, nTest)
    val resultTest = computeGold(matrixATest, matrixBTest)

    //Execute kernel
    val scalaFun = kernel.as[ScalaFunction `(` Int `,` Int `,` Int `,` scala.Array[scala.Array[Float]] `,`
      scala.Array[scala.Array[Float]] `)=>` scala.Array[Float]].withSizes(LocalSize(1), GlobalSize(32))

    val (result, _) = scalaFun(mTest `,` nTest `,` kTest `,` matrixATest `,` matrixBTest)

    val resultMatrix = result.sliding(mTest, nTest).toArray

    //Check result
    if (!similar(resultTest, resultMatrix)) {
      println("Expected:")
      print(resultTest.map(_.mkString(" ")).mkString("\n"))
      println()
      println("\nFound:")
      print(resultMatrix.map(_.mkString(" ")).mkString("\n"))
      println()
      throw new Exception("False Result")
    }
  }

  private def generateMatrix(n: Int, m: Int): scala.Array[scala.Array[Float]] = {
    val array = new scala.Array[scala.Array[Float]](n)

    for (i <- 0 until n) {
      array(i) = new scala.Array[Float](m)
      for (j <- 0 until m) {
        array(i)(j) = (i * j).asInstanceOf[Float];
      }
    }

    array
  }

  /**
    * Multiply matrixA with matrixB using scala.
    *
    * @param matrixA first matrix
    * @param matrixB second matrix
    * @return product of matrixA and matrixB
    */
  private def computeGold(matrixA: scala.Array[scala.Array[Float]], matrixB: scala.Array[scala.Array[Float]])
  : scala.Array[scala.Array[Float]] = {
    assert(matrixA.transpose.length == matrixB.length)

    matrixA.map(rowA =>
      matrixB.transpose.map(columnB =>
        (rowA zip columnB
          map Function.tupled(_ * _)).sum))
  }
}
