package rise.eqsat

import PatternDSL._
import rise.core.{primitives => rcp}

object rules {
  type Rule = Rewrite[DefaultAnalysisData]

  def containsIdent(v: PatternVar, ident: Var)
                   (egraph: EGraph[DefaultAnalysisData], eclass: EClassId, subst: Subst): Boolean =
    egraph.getMut(subst(v)).data.free.contains(ident.index)

  def neg[D](cond: (EGraph[D], EClassId, Subst) => Boolean)
            (egraph: EGraph[D], eclass: EClassId, subst: Subst): Boolean =
    !cond(egraph, eclass, subst)

  // TODO: find a way to combine different analysis requirements?

  // -- reduction --

  val beta: Rule = Rewrite.init("beta",
    app(lam(?(0)), ?(1)).compile()
      -->
    BetaApplier(?(0), ?(1))
  )
  val betaNat: Rule = Rewrite.init("beta-nat",
    nApp(nLam(?(0)), `?n`(0)).compile()
      -->
    BetaNatApplier(?(0), `?n`(0))
  )
  val eta: Rule = Rewrite.init("eta",
    lam(app(?(0), %(0))).compile()
      -->
    ConditionalApplier(neg(containsIdent(?(0), %(0))), Set(?(0)),
      ShiftedApplier(?(0), ?(1), (-1, 0, 0), (1, 0, 0),
        ?(1): Pattern))
  )

  import rise.core.types.{Nat, DataType, Type}
  import NamedRewriteDSL._

  /*val eta: Rule = NamedRewrite.init("eta",
    lam("x", app("f", "x")) --> "f"
  ) when neg(containsIdent("f", "x"))*/
  val removeTransposePair: Rule = NamedRewrite.init("remove-transpose-pair",
    app(transpose, app(transpose, "x")) --> "x"
  )

  // -- algorithmic --

  val mapFusion: Rule = NamedRewrite.init("map-fusion",
    app(app(map, "f"), app(app(map, "g"), "in"))
      -->
    app(app(map, lam("x", app("f", app("g", "x")))), "in")
  )
  val mapFission: Rule = NamedRewrite.init("map-fission",
    app(map, lam("x", app("f", "gx" :: ("dt": DataType))))
      -->
    lam("in", app(app(map, "f"), app(app(map, lam("x", "gx")), "in")))
  ) when neg(containsIdent(?(0), %(0)))
  // TODO: neg(containsIdent("f", "x"))

  // FIXME: how can we generalize to N?
  val mapOutsideMakeArray2: Rule = NamedRewrite.init("map-outside-make-array-2",
    app(app(rcp.makeArray(2).primitive, app(app(map, "f1"), "e")), app(app(map, "f2"), "e"))
      -->
    app(transpose, app(app(map, lam("x", app(app(rcp.makeArray(2).primitive, app("f1", "x")), app("f2", "x")))), "e"))
  )

  // - slide widening -

  val dropInSlide: Rule = NamedRewrite.init("drop-in-slide",
    app(nApp(drop, "l"), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(drop, "l")), app(nApp(nApp(slide, ("n": Nat) + ("l": Nat)), 1), "in"))
  )
  val takeInSlide: Rule = NamedRewrite.init("take-in-slide",
    app(nApp(take, "r") :: ((("s": Nat)`.``_`) ->: `_`), app(nApp(nApp(slide, "n"), 1), "in"))
      -->
    app(app(map, nApp(take, "n")), app(nApp(nApp(slide, ("n": Nat) + ("s": Nat) - ("r": Nat)), 1), "in"))
  )

  // -- movement --

  val mapSlideBeforeTranspose: Rule = NamedRewrite.init("map-slide-before-transpose",
    app(transpose, app(app(map, nApp(nApp(slide, "sz"), "sp")), "in"))
      -->
    app(app(map, transpose), app(nApp(nApp(slide, "sz"), "sp"), app(transpose, "in")))
  )
  val slideBeforeMapMapF: Rule = NamedRewrite.init("slide-before-map-map-f",
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
      -->
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
  )
  val slideBeforeMap: Rule = NamedRewrite.init("slide-before-map",
    app(nApp(nApp(slide, "sz"), "sp"), app(app(map, "f"), "in"))
      -->
    app(app(map, app(map, "f")), app(nApp(nApp(slide, "sz"), "sp"), "in"))
  )

  val dropBeforeMap: Rule = NamedRewrite.init("drop-before-map",
    app(nApp(drop, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(drop, "n"), "in"))
  )
  val takeBeforeMap: Rule = NamedRewrite.init("take-before-map",
    app(nApp(take, "n"), app(app(map, "f"), "in"))
      -->
    app(app(map, "f"), app(nApp(take, "n"), "in"))
  )

  val dropBeforeTake: Rule = NamedRewrite.init("drop-before-take",
    app(nApp(drop, "m"), app(nApp(take, "n+m"), "in"))
      -->
    app(nApp(take, ("n+m": Nat) - ("m": Nat)), app(nApp(drop, "m"), "in"))
  )

  // -- lowering --

  val reduceSeq: Rule = NamedRewrite.init("reduce-seq",
    reduce --> rcp.reduceSeq.primitive
  )
  val reduceSeqUnroll: Rule = NamedRewrite.init("reduce-seq-unroll",
    reduce --> rcp.reduceSeqUnroll.primitive
  )
  val mapSeq: Rule = NamedRewrite.init("map-seq",
    map --> rcp.mapSeq.primitive
  )
  val mapSeqUnroll: Rule = NamedRewrite.init("map-seq-unroll",
    map --> rcp.mapSeqUnroll.primitive
  )
  val iterateStream: Rule = NamedRewrite.init("iterate-stream",
    map --> rcp.iterateStream.primitive
  )
  val toMemAfterMapSeq: Rule = NamedRewrite.init("to-mem-after-map-seq",
    app(app(rcp.mapSeq.primitive, "f"), "in")
      -->
    app(rcp.toMem.primitive, app(app(rcp.mapSeq.primitive, "f"), "in"))
  )

  // TODO: generalize to circularBuffer(load: Expr) and rotateValues(write: Expr)
  val circularBufferScalar: Rule = NamedRewrite.init("circular-buffer-scalar",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(nApp(rcp.circularBuffer.primitive, "sz"), "sz"), lam("x", "x"))
  )
  val rotateValuesScalar: Rule = NamedRewrite.init("rotate-values-scalar",
    nApp(nApp(slide, "sz"), 1) --> app(nApp(rcp.rotateValues.primitive, "sz"), lam("x", "x"))
  )
}
