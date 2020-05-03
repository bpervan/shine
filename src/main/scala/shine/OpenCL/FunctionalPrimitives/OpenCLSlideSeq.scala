package shine.OpenCL.FunctionalPrimitives

import arithexpr.arithmetic.SimplifiedExpr
import rise.core.{primitives => lp}
import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.{Map, Slide}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.{Data, Store}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.OpenCLSlideSeqIValues
import shine.macros.Primitive.expPrimitive

// performs a sequential slide, taking advantage of the space/time overlapping
// reuse opportunity
@expPrimitive
final case class OpenCLSlideSeq(rot: lp.SlideSeq.Rotate,
                                a: AddressSpace,
                                n: Nat,
                                sz: Nat,
                                sp: Nat,
                                dt1: DataType,
                                dt2: DataType,
                                write_dt1: Phrase[ExpType ->: ExpType],
                                f: Phrase[ExpType ->: ExpType],
                                input: Phrase[ExpType])extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  write_dt1 :: expT(dt1, read) ->: expT(dt1, write)
  f :: expT(sz`.`dt1, read) ->: expT(dt2, write)
  input :: expT(inputSize`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    val I = rot match {
      case lp.SlideSeq.Values => OpenCLSlideSeqIValues.apply _
      case lp.SlideSeq.Indices => ??? // SlideSeqIIndices.apply _
    }
    con(input)(fun(expT(inputSize`.`dt1, read))(x =>
      I(a, n, sz, sp, dt1, dt2,
        fun(expT(dt1, read))(x =>
          fun(accT(dt1))(o => acc(write_dt1(x))(o))),
        fun(expT(sz`.`dt1, read))(x =>
          fun(accT(dt2))(o => acc(f(x))(o))),
        x, A
      )))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = ???

  override def eval(s: Store): Data =
    Map(n, ArrayType(sz, dt1), dt2, f, Slide(n, sz, sp, dt1, input)).eval(s)
}
