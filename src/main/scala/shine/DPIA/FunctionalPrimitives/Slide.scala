package shine.DPIA.FunctionalPrimitives

import arithexpr.arithmetic.SimplifiedExpr
import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{ArrayData, Store}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

import scala.language.reflectiveCalls

@expPrimitive
final case class Slide(n: Nat,
                       sz: Nat,
                       sp: Nat,
                       dt: DataType,
                       input: Phrase[ExpType]) extends ExpPrimitive
{
  val inputSize: Nat with SimplifiedExpr = sp * n + sz - sp

  input :: expT(inputSize`.`dt, read)
  override val t: ExpType = expT(n`.`(sz`.`dt), read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] =
    TranslationToImperative.con(this)(λ(expT(n`.`(sz`.`dt), read))(x =>
      A :=|(n`.`(sz`.`dt))| x ))

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] =
    TranslationToImperative.con(input)(λ(expT(inputSize`.`dt, read))(x =>
      C(Slide(n, sz, sp, dt, x)) ))

  override def eval(s: Store): OperationalSemantics.Data = {
    OperationalSemantics.eval(s, input) match {
      case ArrayData(arrayE) =>

        def slide[T](sz: Int, sp: Int, vector: Vector[T]): Vector[Vector[T]] = {
          val builder = Vector.newBuilder[Vector[T]]
          var vec = vector
          while (vec.nonEmpty) {
            builder += vec.take(sz)
            vec = vec.drop(sp)
          }
          builder.result()
        }

        ArrayData(slide(sz.eval, sp.eval, arrayE).map(ArrayData))

      case _ => throw new Exception("This should not happen")
    }
  }
}
