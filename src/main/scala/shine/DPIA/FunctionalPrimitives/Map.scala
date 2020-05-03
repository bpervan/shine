package shine.DPIA.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.{acc, con, fedAcc}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{MapAcc, MapRead}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class Map(n: Nat,
                     dt1: DataType,
                     dt2: DataType,
                     f: Phrase[ExpType ->: ExpType],
                     array: Phrase[ExpType]) extends  ExpPrimitive
{
  f :: expT(dt1, read) ->: expT(dt2, write)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(n`.`dt2, write)

  override def fedeTranslation(env: scala.Predef.Map[Identifier[ExpType],
                               Identifier[AccType]])
                              (C: Phrase[AccType ->: AccType]
                              ) : Phrase[AccType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt1, read))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    fedAcc(env)(array)(λ(env.toList.head._2.t)(y =>
      MapAcc(n, dt2, dt1,
        Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
        C(y))))
  }

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    val x = Identifier(freshName("fede_x"), ExpType(dt1, read))

    val otype = AccType(dt2)
    val o = Identifier(freshName("fede_o"), otype)

    acc(array)(MapAcc(n, dt2, dt1,
      Lambda(o,fedAcc(scala.Predef.Map((x, o)))(f(x))(λ(otype)(x => x))),
      A))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    con(array)(λ(expT(n`.`dt1, read))(x =>
      C(MapRead(n, dt1, dt2,
        fun(expT(dt1, read))(a =>
          fun(expT(dt2, read) ->: (comm: CommType))(cont =>
            con(f(a))(fun(expT(dt2, read))(b => Apply(cont, b))))),
        x))))
  }

  override def eval(s: Store): Data = {
    val fE = OperationalSemantics.eval(s, f)
    OperationalSemantics.eval(s, array) match {
      case ArrayData(xs) =>
        ArrayData(xs.map { x =>
          OperationalSemantics.eval(s, fE(Literal(x)))
        })

      case _ => throw new Exception("This should not happen")
    }
  }
}
