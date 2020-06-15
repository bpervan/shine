package shine.DPIA

import rise.core.{semantics => rs, types => rt}
import rise.{core => r}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.{OperationalSemantics => OpSem}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA.{FunctionalPrimitives => DPIAPrimitives}
import shine.OpenCL.Primitives.OpenCL
import shine.OpenCL.Primitives.OpenCL.To
import shine.OpenCL.{Global, Local, WorkGroup}

//noinspection RedundantDefaultArgument
object fromRise {
  def apply(expr: r.Expr): Phrase[_ <: PhraseType] = {
    if (!r.IsClosedForm(expr)) {
      throw new Exception(s"expression is not in closed form: $expr")
    }
    expression(expr)
  }

  def expression(expr: r.Expr): Phrase[_ <: PhraseType] = expr match {
    case r.Identifier(name) =>
      Identifier(name, `type`(expr.t))

    case r.Lambda(x, e) => expr.t match {
      case rt.FunType(i, _) =>
        Lambda(Identifier(x.name, `type`(i)), expression(e))
      case _ => error(expr.t.toString, "a function type")
    }

    case r.App(f, e) =>
      val ef = expression(f)
        .asInstanceOf[Phrase[FunType[PhraseType, PhraseType]]]
      val ee = expression(e).asInstanceOf[Phrase[PhraseType]]
      Apply(ef, ee)

    case r.DepLambda(x, e) => x match {
      case n: rt.NatIdentifier =>
        DepLambda[NatKind](natIdentifier(n))(expression(e))
      case dt: rt.DataTypeIdentifier =>
        DepLambda[DataKind](dataTypeIdentifier(dt))(expression(e))
      case a: rt.AddressSpaceIdentifier =>
        DepLambda[AddressSpaceKind](addressSpaceIdentifier(a))(expression(e))
    }

    case r.DepApp(f, x) =>
      def depApp[K <: Kind](f: r.Expr, arg: K#T): DepApply[K, PhraseType] =
        DepApply[K, PhraseType](
          expression(f).asInstanceOf[Phrase[DepFunType[K, PhraseType]]],
          arg)
      x match {
        case n: Nat => depApp[NatKind](f, n)
        case dt: rt.DataType =>
          r.lifting.liftDepFunExpr[rise.core.types.DataKind](f) match {
            case r.lifting.Reducing(r) => expression(r(dt))
            case _ => depApp[DataKind](f, dataType(dt))
          }
        case a: rt.AddressSpace => depApp[AddressSpaceKind](f, addressSpace(a))
      }

    case r.Literal(d) => d match {
      case rs.NatData(n) => Natural(n)
      case rs.IndexData(i, n) => DPIAPrimitives.AsIndex(n, Natural(i))
      case _ => Literal(data(d))
    }

    case p: r.Primitive => primitive(p, expr.t)
  }

  def addressSpace(a: rt.AddressSpace): AddressSpace = a match {
    case rt.AddressSpace.Global => AddressSpace.Global
    case rt.AddressSpace.Local => AddressSpace.Local
    case rt.AddressSpace.Private => AddressSpace.Private
    case rt.AddressSpace.Constant => AddressSpace.Constant
    case rt.AddressSpaceIdentifier(name, _) => AddressSpaceIdentifier(name)
  }

  def scalarType(t: rt.ScalarType): ScalarType = t match {
    case rt.bool => bool
    case rt.int => int
    case rt.i8 => i8
    case rt.i16 => i16
    case rt.i32 => i32
    case rt.i64 => i64
    case rt.u8 => u8
    case rt.u16 => u16
    case rt.u32 => u32
    case rt.u64 => u64
    case rt.f16 => f16
    case rt.f32 => f32
    case rt.f64 => f64
    case rt.NatType => NatType
  }

  def basicType(t: rt.BasicType): BasicType = t match {
    case st: rt.ScalarType => scalarType(st)
    case rt.IndexType(sz) => IndexType(sz)
    case rt.VectorType(sz, et) => et match {
      case e : rt.ScalarType => VectorType(sz, scalarType(e))
      case _ => ???
    }
  }

  def dataType(t: rt.DataType): DataType = t match {
    case bt: rt.BasicType => basicType(bt)
    case i: rt.DataTypeIdentifier => dataTypeIdentifier(i)
    case rt.ArrayType(sz, et) => ArrayType(sz, dataType(et))
    case rt.DepArrayType(sz, f) => DepArrayType(sz, ntd(f))
    case rt.PairType(a, b) => PairType(dataType(a), dataType(b))
    case rt.NatToDataApply(f, n) => NatToDataApply(ntd(f), n)
  }

  def ntd(ntd: rt.NatToData): NatToData= ntd match {
    case rt.NatToDataLambda(n, body) =>
      NatToDataLambda(natIdentifier(n), dataType(body))
    case rt.NatToDataIdentifier(x, _) => NatToDataIdentifier(x)
  }

  def ntn(ntn: rt.NatToNat): NatToNat= ntn match {
    case rt.NatToNatLambda(n, body) => NatToNatLambda(natIdentifier(n), body)
    case rt.NatToNatIdentifier(x, _) => NatToNatIdentifier(x)
  }

  def dataTypeIdentifier(dt: rt.DataTypeIdentifier): DataTypeIdentifier =
    DataTypeIdentifier(dt.name)
  def natIdentifier(n: rt.NatIdentifier): NatIdentifier =
    NatIdentifier(n.name, n.range)
  def addressSpaceIdentifier(a: rt.AddressSpaceIdentifier): AddressSpaceIdentifier =
    AddressSpaceIdentifier(a.name)
  def natToNatIdentifier(n: rt.NatToNatIdentifier): NatToNatIdentifier =
    NatToNatIdentifier(n.name)
  def natToDataIdentifier(n: rt.NatToDataIdentifier): NatToDataIdentifier =
    NatToDataIdentifier(n.name)

  def `type`(ty: rt.Type): PhraseType = ty match {
    case dt: rt.DataType => ExpType(dataType(dt), read)
    case rt.FunType(i, o) => `type`(i) ->: `type`(o)
    case rt.DepFunType(i, t) => i match {
      case dt: rt.DataTypeIdentifier =>
        dataTypeIdentifier(dt) ->: `type`(t)
      case n: rt.NatIdentifier =>
        natIdentifier(n) ->: `type`(t)
      case n2n: rt.NatToNatIdentifier =>
        natToNatIdentifier(n2n) ->: `type`(t)
      case n2d: rt.NatToDataIdentifier =>
        natToDataIdentifier(n2d) ->: `type`(t)
    }
    case rt.TypeIdentifier(_) | rt.TypePlaceholder =>
      throw new Exception("This should not happen")
  }

  def data(d: rs.Data): OpSem.Data = d match {
    case rs.ArrayData(a) => OpSem.ArrayData(a.map(data).toVector)
    case rs.PairData(a, b) => OpSem.PairData(data(a), data(b))
    case rs.BoolData(b) => OpSem.BoolData(b)
    case rs.IntData(i) => OpSem.IntData(i)
    case rs.FloatData(f) => OpSem.FloatData(f)
    case rs.DoubleData(d) => OpSem.DoubleData(d)
    case rs.VectorData(v) => OpSem.VectorData(v.map(data(_)).toVector)
    case rs.IndexData(i, n) => OpSem.IndexData(i, n)
    case rs.NatData(n) => OpSem.NatData(n)
  }

  import rise.core.{primitives => core}
  import shine.DPIA.FunctionalPrimitives._

  def fun[T <: PhraseType](
    t: T,
    f: Phrase[T] => Phrase[_ <: PhraseType]
  ): Phrase[_ <: PhraseType] = {
    val x = Identifier(freshName("x"), t)
    Lambda(x, f(x))
  }

  def primitive(p: r.Primitive, t: rt.Type): Phrase[_ <: PhraseType] = {
    import rise.OpenCL.{primitives => ocl}
    import rise.OpenMP.{primitives => omp}
    import shine.OpenMP.FunctionalPrimitives._

    (p, t) match {
      case (core.PrintType(msg),
        rt.FunType(lt: rt.DataType, _))
      =>
        val t = dataType(lt)
        fun[ExpType](expT(t, read), e => PrintType(msg, t, e))

      case (core.NatAsIndex(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.NatType, rt.IndexType(_))))
      =>
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](ExpType(NatType, read), e =>
            AsIndex(n, e)))

      case (core.Map(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(DPIAPrimitives.Map.apply, n, la, lb)

      case (core.MapSeq(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(MapSeq(unroll = false)(_, _, _, _, _), n, la, lb)

      case (core.MapSeqUnroll(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(MapSeq(unroll = true)(_, _, _, _, _), n, la, lb)

      case (omp.MapPar(),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(MapPar, n, la, lb)

      case (ocl.MapGlobal(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(OpenCL.Map(Global, dim), n, la, lb)

      case (ocl.MapLocal(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(OpenCL.Map(Local, dim), n, la, lb)

      case (ocl.MapWorkGroup(dim),
      rt.FunType(rt.FunType(_, lb: rt.DataType),
      rt.FunType(rt.ArrayType(n, la: rt.DataType), _)))
      =>
        makeMap(OpenCL.Map(WorkGroup, dim), n, la, lb)

      case (core.DepMapSeq(),
      rt.FunType(
      rt.DepFunType(lk: rt.NatIdentifier, rt.FunType(_, _)),
      rt.FunType(rt.DepArrayType(n, la), rt.DepArrayType(_, lb))))
      =>
        val a = ntd(la)
        val b = ntd(lb)
        val k = natIdentifier(lk)
        fun[`(nat)->:`[ExpType ->: ExpType]](
          k ->: (ExpType(a(k), read) ->: ExpType(b(k), read))
          , f =>
          fun[ExpType](ExpType(DepArrayType(n, a), read), e =>
            DepMapSeq(unroll = false)(n, a, b, f, e)))

      case (core.ReduceSeq(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, read), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ReduceSeq(unroll = false)(n, a, b, f, i, e))))

      case (core.ReduceSeqUnroll(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _ ))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, read), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ReduceSeq(unroll = true)(n, a, b, f, i, e))))

      case (ocl.OclReduceSeq(),
      rt.DepFunType(i: rt.AddressSpaceIdentifier,
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        DepLambda[AddressSpaceKind](i_space)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
            fun[ExpType](expT(b, read), i =>
              fun[ExpType](expT(n`.`a, read), e =>
                OpenCL.ReduceSeq(unroll = false)(n, i_space, a, b, f, i, e)))))

      case (ocl.OclReduceSeqUnroll(),
      rt.DepFunType(i: rt.AddressSpaceIdentifier,
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val i_space = addressSpaceIdentifier(i)
        DepLambda[AddressSpaceKind](i_space)(
          fun[ExpType ->: ExpType ->: ExpType](
            expT(b, read) ->: expT(a, read) ->: expT(b, write), f =>
            fun[ExpType](expT(b, read), i =>
              fun[ExpType](expT(n`.`a, read), e =>
                OpenCL.ReduceSeq(unroll = true)(n, i_space, a, b, f, i, e)))))

      case (core.ScanSeq(),
      rt.FunType(_,
      rt.FunType(lb: rt.DataType,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType ->: ExpType](
          expT(a, read) ->: expT(b, read) ->: expT(b, write), f =>
          fun[ExpType](expT(b, write), i =>
            fun[ExpType](expT(n`.`a, read), e =>
              ScanSeq(n, a, b, f, i, e))))

      case (core.DepJoin(),
      rt.FunType(rt.DepArrayType(n, llenF), rt.ArrayType(_, la)))
      =>
        val a = dataType(la)
        val lenF: NatToNatLambda = ??? // fromLift(llenF)
        fun[ExpType](expT(n`.d`{ i => lenF(i)`.`a }, read), e =>
          DepJoin(n, lenF, a, e))

      case (core.Join(),
      rt.FunType(rt.ArrayType(n, rt.ArrayType(m, la)), _))
      =>
        val a = dataType(la)
        val w = read // TODO
        fun[ExpType](expT(n`.`(m`.`a), w), e =>
          Join(n, m, w, a, e))

      case (core.Split(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, la), rt.ArrayType(m, _))))
      =>
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, w), e =>
            Split(n, m, w, a, e)))

      case (core.Slide(),
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(insz, la), rt.ArrayType(n, _)))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(sz))(
          DepLambda[NatKind](natIdentifier(sp))(
            fun[ExpType](expT(insz`.`a, read), e =>
              Slide(n, sz, sp, a, e))))

      case (core.SlideSeq(rot),
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(insz, ls), rt.ArrayType(n, lt)))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        DepLambda[NatKind](natIdentifier(sz))(
          DepLambda[NatKind](natIdentifier(sp))(
            fun[ExpType ->: ExpType](
              expT(s, read) ->: expT(s, write), write_dt1 =>
              fun[ExpType ->: ExpType](
                expT(sz`.`s, read) ->: expT(t, write), f =>
                fun[ExpType](expT(insz`.`s, read), e =>
                  SlideSeq(rot, n, sz, sp, s, t, write_dt1, f, e))))))

      case (ocl.OclSlideSeq(rot),
      rt.DepFunType(la: rt.AddressSpaceIdentifier,
      rt.DepFunType(sz: rt.NatIdentifier,
      rt.DepFunType(sp: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(insz, ls), rt.ArrayType(n, lt))))))))
      =>
        val s = dataType(ls)
        val t = dataType(lt)
        val a = addressSpaceIdentifier(la)
        DepLambda[AddressSpaceKind](a)(
        DepLambda[NatKind](natIdentifier(sz))(
        DepLambda[NatKind](natIdentifier(sp))(
          fun[ExpType ->: ExpType](
            expT(s, read) ->: expT(s, write), write_dt1 =>
            fun[ExpType ->: ExpType](
              expT(sz`.`s, read) ->: expT(t, write), f =>
              fun[ExpType](expT(insz`.`s, read), e =>
                OpenCL.SlideSeq(rot)(a, n, sz, sp, s, t, write_dt1, f, e)))))))

      case (core.Reorder(),
      rt.FunType(_,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        fun[ExpType ->: ExpType](
          expT(idx(n), read) ->: expT(idx(n), read), idxF =>
          fun[ExpType ->: ExpType](
            expT(idx(n), read) ->: expT(idx(n), read), idxFinv =>
            fun[ExpType](expT(n`.`a, read), e =>
              Reorder(n, a, idxF, idxFinv, e))))

      case (core.Gather(),
      rt.FunType(rt.ArrayType(m, _),
      rt.FunType(rt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        fun[ExpType](expT(m`.`idx(n), read), y =>
          fun[ExpType](expT(n`.`a, read), x =>
            Gather(n, m, a, y, x)))

      case (core.Transpose(),
      rt.FunType(rt.ArrayType(n, rt.ArrayType(m, la)), _))
      =>
        val a = dataType(la)
/* FIXME?

        val transposeFunction =
          λ(ExpType(IndexType(n * m), read))(i => {
            AsIndex(n * m, mapTransientNat(i, j => {
              val col = (j % n) * m
              val row = j / n
              row + col
            }))
          })

        val transposeInverseFunction =
          λ(ExpType(IndexType(n * m), read))(i => {
            AsIndex(n * m, mapTransientNat(i, j => {
              val col = (j % m) * n
              val row = j / m
              row + col
            }))
          })

        fun[ExpType](exp"[$n.$m.$a, $read]", e =>
          Split(n, m, read, a,
            Reorder(n * m, a, transposeFunction, transposeInverseFunction,
              Join(n, m, read, a, e))))

 */
        fun[ExpType](expT(n`.`(m`.`a), read), e =>
          Transpose(n, m, a, e))

      case (core.Take(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(nm, la), lw)))
      =>
        val m = nm - n
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(nm`.`a, w), e =>
            Take(n, m, w, a, e)))

      case (core.Drop(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(nm, la), _)))
      =>
        val m = nm - n
        val a = dataType(la)
        val w = read // TODO
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(nm`.`a, w), e =>
            Drop(n, m, w, a, e)))

      case (core.PadCst(),
      rt.DepFunType(l: rt.NatIdentifier,
      rt.DepFunType(r: rt.NatIdentifier,
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _)))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(l))(
          DepLambda[NatKind](natIdentifier(r))(
            fun[ExpType](expT(a, read), cst =>
                fun[ExpType](expT(n`.`a, read), e =>
                  Pad(n, l, r, a, cst, e)))))

      case (core.PadClamp(),
      rt.DepFunType(l: rt.NatIdentifier,
      rt.DepFunType(r: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(n, la), _))))
      =>
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(l))(
          DepLambda[NatKind](natIdentifier(r))(
              fun[ExpType](expT(n`.`a, read), e =>
                PadClamp(n, l, r, a, e))))

      case (core.Unzip(),
      rt.FunType(
      rt.ArrayType(n, rt.PairType(la, lb)),
      rt.PairType(rt.ArrayType(_, _), rt.ArrayType(_, _))))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(n`.`(a x b), read), e =>
            Unzip(n, a, b, e))

      case (core.Zip(),
      rt.FunType(rt.ArrayType(n, la),
      rt.FunType(rt.ArrayType(_, lb), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(n`.`a, read), x =>
          fun[ExpType](expT(n`.`b, read), y =>
            Zip(n, a, b, x, y)))

      case (core.Fst(),
      rt.FunType(rt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a x b, read), e => Fst(a, b, e))

      case (core.MapFst(),
      rt.FunType(rt.FunType(la: rt.DataType, la2: rt.DataType),
      rt.FunType(rt.PairType(_, lb), _)))
      =>
        val a = dataType(la)
        val a2 = dataType(la2)
        val b = dataType(lb)
        fun[ExpType ->: ExpType](expT(a, read) ->: expT(a2, read), f =>
          fun[ExpType](expT(a x b, read), e => MapFst(a, b, a2, f, e)))

      case (core.Snd(),
      rt.FunType(rt.PairType(la, lb), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a x b, read), e => Snd(a, b, e))

      case (core.MapSnd(),
      rt.FunType(rt.FunType(lb: rt.DataType, lb2: rt.DataType),
      rt.FunType(rt.PairType(la, _), _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        val b2 = dataType(lb2)
        fun[ExpType ->: ExpType](expT(b, read) ->: expT(b2, read), f =>
          fun[ExpType](expT(a x b, read), e => MapSnd(a, b, b2, f, e)))

      case (core.Pair(),
      rt.FunType(la: rt.DataType,
      rt.FunType(lb: rt.DataType, _)))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType](expT(a, read), x =>
          fun[ExpType](expT(b, read), y =>
            Pair(a, b, x, y)))

      case (core.Idx(),
      rt.FunType(_,
      rt.FunType(rt.ArrayType(n, la), _)))
      =>
        val a = dataType(la)
        fun[ExpType](expT(idx(n), read), i =>
          fun[ExpType](expT(n`.`a, read), e =>
            DPIAPrimitives.Idx(n, a, i, e)))

      case (core.Select(),
      rt.FunType(_,
      rt.FunType(la: rt.DataType, _)))
      =>
        val a = dataType(la)
        fun[ExpType](ExpType(bool, read), c =>
          fun[ExpType](ExpType(a, read), tExpr =>
            fun[ExpType](ExpType(a, read), fExpr =>
              IfThenElse(c, tExpr, fExpr))))

      case (core.Neg(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e => UnaryOp(Operators.Unary.NEG, e))
      case (core.Not(), _) =>
        fun[ExpType](expT(bool, read), e => UnaryOp(Operators.Unary.NOT, e))

      case (core.Add(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.ADD, e1, e2)))
      case (core.Sub(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.SUB, e1, e2)))
      case (core.Mul(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.MUL, e1, e2)))
      case (core.Div(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.DIV, e1, e2)))
      case (core.Mod(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.MOD, e1, e2)))

      case (core.Gt(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.GT, e1, e2)))
      case (core.Lt(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.LT, e1, e2)))
      case (core.Equal(), rt.FunType(la: rt.DataType, _)) =>
        val a = dataType(la)
        fun[ExpType](expT(a, read), e1 =>
          fun[ExpType](expT(a, read), e2 => BinOp(Operators.Binary.EQ, e1, e2)))

      case (core.Cast(), rt.FunType(la: rt.BasicType, lb: rt.BasicType))
      =>
        val a = basicType(la)
        val b = basicType(lb)
        fun[ExpType](ExpType(a, read), x =>
          Cast(a, b, x))

      case (core.Let(), rt.FunType(rt.FunType(la: rt.DataType, lb: rt.DataType), _))
      =>
        val a = dataType(la)
        val b = dataType(lb)
        fun[ExpType ->: ExpType](expT(a, read) ->: expT(b, read), f =>
          fun[ExpType](ExpType(a, read), x =>
            Let(a, b, x, f)))

      case (f @ r.ForeignFunction(decl), _)
      =>
        val (inTs, outT) = foreignFunIO(f.t)
        wrapForeignFun(decl, inTs, outT, Vector())

      case (core.Generate(), rt.FunType(_, rt.ArrayType(n, la)))
      =>
        val a = dataType(la)
        fun[ExpType ->: ExpType](expT(idx(n), read) ->: expT(a, read), f =>
          Generate(n, a, f))

      case (core.MakeArray(_), lt) =>
        wrapArray(lt, Vector())

      case (core.Iterate(),
      rt.DepFunType(k: rt.NatIdentifier,
      rt.FunType(rt.DepFunType(ll: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(ln, _), _)),
      rt.FunType(rt.ArrayType(insz, _), rt.ArrayType(m, la)))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val a = dataType(la)
        DepLambda[NatKind](natIdentifier(k))(
          fun[`(nat)->:`[ExpType ->: ExpType]](
            l ->: (expT(ln`.`a, read) ->: expT(l`.`a, read)), f =>
              fun[ExpType](expT(insz`.`a, read), e =>
                Iterate(n, m, k, a, f, e))))

      case (ocl.OclIterate(),
      rt.DepFunType(la: rt.AddressSpaceIdentifier,
      rt.DepFunType(k: rt.NatIdentifier,
      rt.FunType(rt.DepFunType(ll: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(ln, _), _)),
      rt.FunType(rt.ArrayType(insz, _), rt.ArrayType(m, ldt))))))
      =>
        val l = natIdentifier(ll)
        val n = ln /^ l
        val dt = dataType(ldt)
        val a = addressSpaceIdentifier(la)
        DepLambda[AddressSpaceKind](a)(
          DepLambda[NatKind](natIdentifier(k))(
            fun[`(nat)->:`[ExpType ->: ExpType]](
              l ->: (expT(ln`.`dt, read) ->: expT(l`.`dt, read)), f =>
              fun[ExpType](expT(insz`.`dt, read), e =>
                OpenCL.Iterate(a, n, m, k, dt, f, e)))))

      case (core.AsVector(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, la: rt.ScalarType), rt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, read), e =>
            AsVector(n, m, a, e)))

      case (core.AsVectorAligned(),
      rt.DepFunType(n: rt.NatIdentifier,
      rt.FunType(rt.ArrayType(mn, la: rt.ScalarType), rt.ArrayType(m, _))))
      =>
        val a = scalarType(la)
        DepLambda[NatKind](natIdentifier(n))(
          fun[ExpType](expT(mn`.`a, read), e =>
            AsVectorAligned(n, m, a, e)))

      case (core.AsScalar(),
      rt.FunType(rt.ArrayType(m, rt.VectorType(n, la: rt.ScalarType)), _))
      =>
        val a = scalarType(la)
        fun[ExpType](ExpType(ArrayType(m, VectorType(n, a)), read), e =>
          AsScalar(m, n, a, e))

      case (core.VectorFromScalar(),
      rt.FunType(_, rt.VectorType(n, la: rt.ScalarType)))
      =>
        val a = scalarType(la)
        fun[ExpType](ExpType(a, read), e =>
          VectorFromScalar(n, a, e))

      case (core.IndexAsNat(), rt.FunType(rt.IndexType(n), rt.NatType))
      =>
        fun[ExpType](expT(idx(n), read), e =>
          IndexAsNat(n, e))

      case (ocl.OclToMem(),
      rt.DepFunType(las: rt.AddressSpaceIdentifier,
      rt.FunType(la: rt.DataType, _)))
      =>
        val a = dataType(la)
        val as = addressSpaceIdentifier(las)
        DepLambda[AddressSpaceKind](as)(
          fun[ExpType](expT(a, write), e =>
            To(as, a, e)))

      case (core.Reduce(), _) =>
        throw new Exception(s"$p has no implementation")

      case (p, _) =>
        throw new Exception(s"Missing rule for $p : ${p.t}")
    }
  }

  private def makeMap(map: (Nat, DataType, DataType, Phrase[ExpType ->: ExpType], Phrase[ExpType]) => Phrase[_ <: PhraseType],
                      n: Nat,
                      la: rt.DataType,
                      lb: rt.DataType): Phrase[_ <: PhraseType] = {
    val a = dataType(la)
    val b = dataType(lb)
    fun[ExpType ->: ExpType](ExpType(a, read) ->: ExpType(b, write), f =>
      fun[ExpType](expT(n`.`a, read), e =>
        map(n, a, b, f, e)))
  }

  def foreignFunIO(t: rt.Type): (Vector[DataType], DataType) = {
    t match {
      case lo: rt.DataType =>
        (Vector(), dataType(lo))
      case rt.FunType(laa, lb) => laa match {
        case la: rt.DataType =>
          val a = dataType(la)
          val (i, o) = foreignFunIO(lb)
          (a +: i, o)
        case _ => ???
      }
      case rt.DepFunType(_, _) =>
        throw new Exception("This should not be possible")
      case rt.TypeIdentifier(_) | rt.TypePlaceholder =>
        throw new Exception("This should not happen")
    }
  }

  def wrapForeignFun(decl: r.ForeignFunction.Decl,
                     intTs: Vector[DataType],
                     outT: DataType,
                     args: Vector[Phrase[ExpType]]): Phrase[_ <: PhraseType] = {
    val i = args.length
    if (i < intTs.length) {
      fun[ExpType](ExpType(intTs(i), read), a =>
        wrapForeignFun(decl, intTs, outT, args :+ a))
    } else {
      ForeignFunction(decl, intTs, outT, args)
    }
  }

  def wrapArray(
    t: rt.Type,
    elements: Vector[Phrase[ExpType]]
  ): Phrase[_ <: PhraseType] = {
    t match {
      case rt.ArrayType(_, et) => Array(dataType(et), elements)
      case rt.FunType(in: rt.DataType, t2) =>
        fun[ExpType](ExpType(dataType(in), read), e =>
          wrapArray(t2, elements :+ e))
      case _ => error(s"did not expect $t")
    }
  }
}