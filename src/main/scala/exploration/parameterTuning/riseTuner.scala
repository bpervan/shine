package exploration.parameterTuning

import apps.mm
import apps.mm.{computeGold, runKernel, runOriginal}
import opencl.executor.Executor
import shine.OpenCL.{GlobalSize, LocalSize}
import util.gen

object riseTuner {
  private val N = 64
  private val M = 128
  private val O = 128

  def main(args: Array[String]): Unit = {

    // check argument size to avoid unexpected behavior
    args.size match {
      case 6 => println("ok")
      case _ => System.exit(1)
    }

    println("we are here and here is party")

    // initialize executor
    Executor.loadLibrary()
    Executor.init()

    // parse in values
    val v3 = args(0).toInt
    val v4 = args(1).toInt
    val v5 = args(2).toInt
    val v6 = args(3).toInt
    val v7 = args(4).toInt
    val v8 = args(5).toInt

    // generate kernel
    val kernel = mm.genKernel(v3, v4, v5, v6, v7, v8)

    // compute gold
    val (at, b, gold) = randGold()

    // run
    val runs = Seq(
      "original" -> runOriginal("CGO17_MMNVIDIA.cl",
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b),
      "dpia" -> runKernel(gen.OpenCLKernel(kernel),
        LocalSize((32, 8)), GlobalSize((M/4, N/8)), at, b)
    )

    // check result
    runs.foreach(r => {
      util.assertSame(r._2._1, gold, s"${r._1} is different from gold")
      println(s"${r._1} time: ${r._2._2}")
    })
    println("runtime: " + runs.last._2._2.value)

    // deinitialize executor
    Executor.shutdown()

    // return result
    runs.last._2._2.value
  }

  private def randGold(): (Array[Array[Float]], Array[Array[Float]], Array[Float]) = {
    val rand = new scala.util.Random
    val At = Array.fill(O, N)(rand.nextFloat * 10)
    val B = Array.fill(O, M)(rand.nextFloat * 10)
    val gold = computeGold(N, M, O, At, B).flatten
    (At, B, gold)
  }
}
