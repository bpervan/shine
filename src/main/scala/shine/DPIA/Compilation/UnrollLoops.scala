package shine.DPIA.Compilation

import arithexpr.arithmetic.ArithExpr.isSmaller
import arithexpr.arithmetic.Cst
import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.ImperativePrimitives._
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.Primitives.OpenCL

object UnrollLoops {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val r = VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case For(n, Lambda(ident: Identifier[_], body), true) =>
          Continue(unrollLoop(n, init=0, step=1, i => Phrase.substitute(AsIndex(n, Natural(i)), `for`=ident, in=body)), this)
        case ForNat(n, DepLambda(ident: NatIdentifier, body), true) =>
          Continue(unrollLoop(n, init=0, step=1, i => PhraseType.substitute(i, `for`=ident, in=body)), this)
        case pf : OpenCL.ParFor if pf.unroll => pf.loopBody match {
          case Lambda(ident: Identifier[_], Lambda(identOut: Identifier[_], body)) =>
            pf.out.t.dataType match {
              case ArrayType(_, elemType) =>
                Continue(unrollLoop(pf.n, pf.init, pf.step, i => Phrase.substitute(IdxAcc(pf.n, elemType, AsIndex(pf.n, Natural(i)), pf.out),
                  `for`=identOut,
                  Phrase.substitute(AsIndex(pf.n, Natural(i)), `for`=ident, in=body))), this)
              case _ => throw new Exception("OpenCLParFor acceptor has to be of ArrayType.")
            }
          case _ =>
            Continue(p, this)
        }
        case _ =>
          Continue(p, this)
      }
    })
    r
  }

  def unrollLoop(n: Nat, init: Nat, step: Nat,
                 genBody: Nat => Phrase[CommType]): Phrase[CommType] = {
    import arithexpr.arithmetic.NotEvaluableException

    val stopMax = try {
      n.max.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate ${n.max} during loop unrolling")
    }

    val startMin = try {
      init.min.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate ${init.min} during loop unrolling")
    }

    val incr = try {
      step.eval
    } catch {
      case _: NotEvaluableException => throw new Exception(s"cannot evaluate $step during loop unrolling")
    }

    val numIter = ceilDiv(stopMax - startMin, incr)

    val tmp = (0 until numIter).foldLeft[Phrase[CommType]](Comment(s"unrolling loop of $numIter"))({ case (prev, i) =>
      val index = init + Cst(i * incr)
      assert(isSmaller(index, n).contains(true)) //TODO add if-guards otherwise.
      //TODO store result of init in temporary variable
      Seq(prev, genBody(index))
    })

    tmp
  }

  def ceilDiv(a: Int, b: Int) : Int = {
    (a + b - 1)/ b
  }
}
