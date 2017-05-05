package idealised.DSL

import idealised.Core.OperationalSemantics.{FloatData, IndexData, IntData}
import idealised.Core.TypeInference._
import idealised.Core._
import idealised.LowLevelCombinators.{Assign, Idx, IdxAcc, Seq}
import apart.arithmetic.{ContinuousRange, NamedVar}

import scala.language.implicitConversions

package object typed {

  implicit class BinOps(lhs: Phrase[ExpType]) {
    def +(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.ADD, lhs, rhs)
    def -(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.SUB, lhs, rhs)
    def *(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MUL, lhs, rhs)
    def /(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.DIV, lhs, rhs)
    def %(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.MOD, lhs, rhs)
    def >(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.GT, lhs, rhs)
    def <(rhs: Phrase[ExpType]) = BinOpPhrase(BinOpPhrase.Op.LT, lhs, rhs)
    def unary_- = UnaryOpPhrase(UnaryOpPhrase.Op.NEG, lhs)
  }

  implicit class ExpPhraseExtensions(e: Phrase[ExpType]) {
    def `@`(index: Phrase[ExpType]) = (index.t, e.t) match {
      case (ExpType(IndexType(n1)), ExpType(ArrayType(n2, dt))) if n1 == n2 =>
        Idx(n1, dt, index, e)
      case x => error(x.toString, "(exp[idx(n)], exp[n.dt])")
    }
  }

  implicit class AccPhraseExtensions(a: Phrase[AccType]) {
    def `@`(index: Phrase[ExpType]) = (index.t, a.t) match {
      case (ExpType(IndexType(n1)), AccType(ArrayType(n2, dt))) if n1 == n2 =>
        IdxAcc(n1, dt, index, a)
      case x => error(x.toString, "(exp[idx(n)], acc[n.dt])")
    }
  }

  implicit class Assignment(lhs: Phrase[AccType]) {
    def :=(rhs: Phrase[ExpType]) = {
      (lhs.t, rhs.t) match {
        case (AccType(dt1), ExpType(dt2))
          if dt1 == dt2 =>
            dt1 match {
              case bt: BasicType =>
                Assign(bt, lhs, rhs)
              case _ =>
                error(dt1.toString, expected = "it to be a basic type.")
            }
        case (x1, x2) => error(x1.toString() + " and " + x2.toString(),
          expected = "them to have a matching data type.")
      }
    }
  }

  implicit class CallLambda[T1 <: PhraseType, T2 <: PhraseType](fun: Phrase[T1 -> T2]) {
    def apply(arg: Phrase[T1]): Phrase[T2] = Lift.liftFunction(fun)(arg)

    def $(arg: Phrase[T1]): Phrase[T2] = apply(arg)
  }

  implicit class CallExpLambda[T <: PhraseType](fun: Phrase[ExpType -> T]) {
    def apply(arg: Phrase[ExpType]): Phrase[T] = CallLambda[ExpType, T](fun)(arg)
    def apply(arg: Nat): Phrase[T] = Lift.liftFunctionToNatLambda(fun)(arg)

    def $(arg: Phrase[ExpType]): Phrase[T] = apply(arg)
    def $(arg: Nat): Phrase[T] = apply(arg)
  }

  implicit class CallNatDependentLambda[T <: PhraseType](fun: Phrase[`(nat)->`[T]]) {
    def apply(arg: Nat): Phrase[T] =
      Lift.liftNatDependentFunction(fun)(arg)

    def $(arg: Nat): Phrase[T] = apply(arg)
  }

  implicit class CallTypeDependentLambda[T <: PhraseType](fun: Phrase[`(dt)->`[T]]) {
    def apply(arg: DataType): Phrase[T] =
      Lift.liftTypeDependentFunction(fun)(arg)

    def $(arg: DataType): Phrase[T] = apply(arg)
  }

  implicit class FunComp[T1 <: PhraseType, T2 <: PhraseType](f: Phrase[T1 -> T2]) {
    def o[T3 <: PhraseType](g: Phrase[T3 -> T1]): Phrase[T3 -> T2] = {
      typed.λ(g.t.inT)(arg => f(g(arg)))
    }
  }

  implicit class SequentialComposition(c1: Phrase[CommandType]) {
    def `;`(c2: Phrase[CommandType]): Phrase[CommandType] = Seq(c1, c2)
  }

  implicit class VarExtensions(v: Phrase[VarType]) {
    def rd: Proj1Phrase[ExpType, AccType] = π1(v)

    def wr: Proj2Phrase[ExpType, AccType] = π2(v)
  }

  implicit class IdentExpPhraseExtensions(i: IdentPhrase[ExpType]) {
    def asNatIdentifier = NamedVar(i.name)
    def asNatIdentifier(withUpperBound: Nat) =
      NamedVar(i.name, ContinuousRange(0, withUpperBound))
  }

  implicit class NatExtensions(n: Nat) {
    def asPhrase = LiteralPhrase(IndexData(n), IndexType(n.max))
    def asPhrase(withType: IndexType) = LiteralPhrase(IndexData(n), withType)
  }

  implicit def toLiteralInt(i: Int): LiteralPhrase = LiteralPhrase(IntData(i), int)
  implicit def toLiteralFloat(f: Float): LiteralPhrase = LiteralPhrase(FloatData(f), float)

  implicit def toPair[T1 <: PhraseType, T2 <: PhraseType](pair: (Phrase[T1], Phrase[T2])): PairPhrase[T1, T2] =
    PairPhrase(pair._1, pair._2)

  implicit def toNatDependentLambda[T <: PhraseType](p: Phrase[T]): NatDependentLambdaPhrase[T] =
    _Λ_( l => p )
}