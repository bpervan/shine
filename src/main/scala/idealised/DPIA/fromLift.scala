package idealised.DPIA

import idealised.DPIA.DSL._
import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA.Semantics.{OperationalSemantics => OpSem}
import lift.{core => l}
import lift.core.{types => lt}
import lift.core.{semantics => ls}

object fromLift {
  def apply(expr: l.Expr): Phrase[_ <: PhraseType] = {
    expr match {
      case l.TypedExpr(l.Identifier(name), t) =>
        Identifier(name, fromLift(t))
      case l.TypedExpr(l.Lambda(x, e), lt.FunctionType(i, _)) =>
        Lambda(Identifier(x.name, fromLift(i)), fromLift(e))
      case l.TypedExpr(l.Apply(f, e), _) =>
        Apply(
          fromLift(f).asInstanceOf[Phrase[FunctionType[ExpType, PhraseType]]],
          fromLift(e).asInstanceOf[Phrase[ExpType]])
      case l.TypedExpr(l.NatLambda(n, e), _) =>
        NatDependentLambda(n, fromLift(e))
      case l.TypedExpr(l.NatApply(f, n), _) =>
        NatDependentApply(
          fromLift(f).asInstanceOf[Phrase[NatDependentFunctionType[PhraseType]]],
          n)
      case l.TypedExpr(l.TypeLambda(dt, e), _) =>
        TypeDependentLambda(DataTypeIdentifier(dt.name), fromLift(e))
      case l.TypedExpr(l.TypeApply(f, dt), _) =>
        TypeDependentApply(
          fromLift(f).asInstanceOf[Phrase[TypeDependentFunctionType[PhraseType]]],
          fromLift(dt)
        )
      case l.TypedExpr(l.Literal(d), _) =>
        Literal(fromLift(d))
      case l.TypedExpr(l.Index(n, sz), _) =>
        Literal(OpSem.IndexData(n, IndexType(sz)))
      case l.TypedExpr(l.NatExpr(n), _) =>
        Natural(n)
      case l.TypedExpr(p: l.Primitive, t) => fromLift(p, t)
    }
  }

  def apply(t: lt.DataType): DataType = {
    t match {
      case lt.DataTypeIdentifier(name) => DataTypeIdentifier(name)
      case lt.ArrayType(sz, et) => ArrayType(sz, fromLift(et))
      case lt.DepArrayType(sz, et) => ???
      case lt.TupleType(a, b) => RecordType(fromLift(a), fromLift(b))
      case lt.bool => bool
      case lt.int => int
      case lt.float => float
      case lt.double => double
      case lt.NatType => NatType
      case lt.IndexType(sz) => IndexType(sz)
      // case _ => ???
    }
  }

  def apply(ty: lt.Type): PhraseType = {
    ty match {
      case dt: lt.DataType => ExpType(fromLift(dt))
      case lt.FunctionType(i, o) => FunctionType(fromLift(i), fromLift(o))
      case lt.TypeDependentFunctionType(dt, t) =>
        TypeDependentFunctionType(DataTypeIdentifier(dt.name), fromLift(t))
      case lt.NatDependentFunctionType(n, t) =>
        NatDependentFunctionType(n, fromLift(t))
    }
  }

  def apply(d: ls.Data): OpSem.Data = {
    d match {
      case ls.ArrayData(a) => OpSem.ArrayData(a.map(fromLift(_)).toVector)
      case ls.TupleData(a, b) => OpSem.RecordData(fromLift(a), fromLift(b))
      case ls.BoolData(b) => OpSem.BoolData(b)
      case ls.IntData(i) => OpSem.IntData(i)
      case ls.FloatData(f) => OpSem.FloatData(f)
      case ls.DoubleData(d) => OpSem.DoubleData(d)
      case ls.VectorData(v) => OpSem.VectorData(v.map(fromLift(_)).toVector)
    }
  }

  import lift.core.{primitives => lp}
  import idealised.DPIA.FunctionalPrimitives._

  def apply(p: l.Primitive, t: lt.Type): Phrase[_ <: PhraseType] = {
    def nFun(f: NatIdentifier => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
      val n = lift.arithmetic.NamedVar(freshName("n"))
      NatDependentLambda(n, f(n))
    }

    def tFun(f: DataTypeIdentifier => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
      val dt = DataTypeIdentifier(freshName("dt"))
      TypeDependentLambda(dt, f(dt))
    }

    def fun[T <: PhraseType](t: T,
                             f: Phrase[T] => Phrase[_ <: PhraseType]): Phrase[_ <: PhraseType] = {
      val x = Identifier(freshName("x"), t)
      Lambda(x, f(x))
    }

    (p, t) match {
      case (lp.map,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> ExpType](ExpType(a) -> ExpType(b), f =>
          fun[ExpType](ExpType(ArrayType(n, a)), e =>
            Map(n, a, b, f, e)))

      case (lp.mapSeq,
      lt.FunctionType(lt.FunctionType(_, lb: lt.DataType),
      lt.FunctionType(lt.ArrayType(n, la: lt.DataType), _)))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> ExpType](ExpType(a) -> ExpType(b), f =>
          fun[ExpType](ExpType(ArrayType(n, a)), e =>
            MapSeq(n, a, b, f, e)))

      case (lp.reduceSeq,
      lt.FunctionType(_,
      lt.FunctionType(lb: lt.DataType,
      lt.FunctionType(lt.ArrayType(n, la), _ ))))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType -> (ExpType -> ExpType)](exp"[$a]" -> (exp"[$b]" -> exp"[$b]"), f =>
          fun[ExpType](exp"[$b]", i =>
            fun[ExpType](exp"[$n.$a]", e =>
              ReduceSeq(n, a, b, f, i, e))))

      case (lp.join,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)
        fun[ExpType](ExpType(ArrayType(n, ArrayType(m, a))), e =>
          Join(n, m, a, e))

      case (lp.split,
      lt.NatDependentFunctionType(_,
      lt.FunctionType(_, lt.ArrayType(m, lt.ArrayType(n, la)))))
      =>
        val a = fromLift(la)
        nFun(n =>
          fun[ExpType](ExpType(ArrayType(m * n, a)), e =>
            Split(n, m, a, e)))

      case (lp.slide,
        lt.NatDependentFunctionType(_,
        lt.NatDependentFunctionType(_,
        lt.FunctionType(lt.ArrayType(_, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        nFun(sz => nFun(sp =>
          fun[ExpType](ExpType(ArrayType(sp * n + sz - sp, a)), e =>
            Slide(n, sz, sp, a, e))))

      case (lp.slideSeq,
      lt.NatDependentFunctionType(_,
      lt.NatDependentFunctionType(_,
      lt.FunctionType(lt.ArrayType(_, la), lt.ArrayType(n, _)))))
      =>
        val a = fromLift(la)
        nFun(sz => nFun(sp =>
          fun[ExpType](ExpType(ArrayType(sp * n + sz - sp, a)), e =>
            SlideSeq(n, sz, sp, a, e))))

      case (lp.transpose,
      lt.FunctionType(lt.ArrayType(n, lt.ArrayType(m, la)), _))
      =>
        val a = fromLift(la)

        val transposeFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            })
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m)))(i => {
            mapIndexExpr(i, j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            })
          })

        fun[ExpType](exp"[$n.$m.$a]", e =>
          Split(n, m, a,
            Reorder(n*m, a, transposeFunction, transposeInverseFunction,
              Join(n, m, a, e))))

      case (lp.zip,
        lt.FunctionType(lt.ArrayType(n, la),
        lt.FunctionType(lt.ArrayType(_, lb), _)))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$n.$a]", x =>
          fun[ExpType](exp"[$n.$b]", y =>
            Zip(n, a, b, x, y)))

      case (lp.fst,
        lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Fst(a, b, e))

      case (lp.snd,
      lt.FunctionType(lt.TupleType(la, lb), _))
      =>
        val a = fromLift(la)
        val b = fromLift(lb)
        fun[ExpType](exp"[$a x $b]", e => Snd(a, b, e))

      case (lp.UnaryOp(op),
      lt.FunctionType(la: lt.DataType, _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", e =>
          UnaryOp(uop(op), e))

      case (lp.BinOp(op),
      lt.FunctionType(la: lt.DataType, _))
      =>
        val a = fromLift(la)
        fun[ExpType](exp"[$a]", x =>
          fun[ExpType](exp"[$a]", y =>
            BinOp(bop(op), x, y)))
    }
  }

  // TODO: remove surface language
  val Unary = idealised.SurfaceLanguage.Operators.Unary
  val Binary = idealised.SurfaceLanguage.Operators.Binary

  def uop(op: l.Operators.Unary.Value): Unary.Value = {
    import l.Operators.Unary._
    op match {
      case NEG => Unary.NEG
    }
  }

  def bop(op: l.Operators.Binary.Value): Binary.Value = {
    import l.Operators.Binary._
    op match {
      case ADD => Binary.ADD
      case SUB => Binary.SUB
      case MUL => Binary.MUL
      case DIV => Binary.DIV
      case MOD => Binary.MOD
      case GT => Binary.GT
      case LT => Binary.LT
      case EQ => Binary.EQ
    }
  }
}