package elevate

import elevate.rise._
import elevate.rise.strategies.normalForm.BENF
import _root_.lift.core.primitives._
import _root_.lift.core._
import _root_.lift.core.types._
import _root_.lift.core.TypedDSL._

package object util {

  // Rise-related utils

  def betaEtaEquals(a: Rise, b: Rise): Boolean = BENF(makeClosed(a)._1).get == BENF(makeClosed(b)._1).get

  val tileSize = 4

  def makeClosed(e: Rise): (Rise, Int) = {
    import _root_.lift.core.types.infer._
    import scala.collection.immutable.Map
    val emptySubs: (Map[Type, Type],
      Map[NatIdentifier, Nat],
      Map[AddressSpaceIdentifier, AddressSpace],
      Map[NatToDataIdentifier, NatToData]) = (Map(), Map(), Map(), Map())
    val (expr, (ts, ns, as, n2ds)) = TopLevel.getFTVs(e.t).foldLeft((e, emptySubs))((acc, ftv) => acc match {
      case (expr, (ts, ns, as, n2ds)) => ftv match {
        case i: TypeIdentifier =>
          val dt = DataTypeIdentifier(freshName("dt"), isExplicit = true)
          (DepLambda[DataKind](dt, expr)(DepFunType[DataKind, Type](dt, expr.t)),
            (ts ++ Map(i -> dt), ns, as , n2ds))
        case i: DataTypeIdentifier =>
          val dt = i.asExplicit
          (DepLambda[DataKind](dt, expr)(DepFunType[DataKind, Type](dt, expr.t)),
            (ts ++ Map(i -> dt), ns, as , n2ds))
        case i: NatIdentifier =>
          val n = i.asExplicit
          (DepLambda[NatKind](n, expr)(DepFunType[NatKind, Type](n, expr.t)),
            (ts, ns ++ Map(i -> n), as, n2ds))
        case i: AddressSpaceIdentifier =>
          val a = i.asExplicit
          (DepLambda[AddressSpaceKind](a, expr)(DepFunType[AddressSpaceKind, Type](a, expr.t)),
            (ts, ns, as ++ Map(i -> a), n2ds))
        case i: NatToDataIdentifier =>
          val n2d = i.asExplicit
          (DepLambda[NatToDataKind](n2d, expr)(DepFunType[NatToDataKind, Type](n2d, expr.t)),
            (ts, ns, as, n2ds ++ Map(i -> n2d)))
        case i => throw TypeException(s"${i.getClass} is not supported yet")
      }
    })
    (new Solution(ts, ns, as, n2ds)(expr), ts.size + ns.size + as.size + n2ds.size)
  }

  // notation
  def T: TDSL[Transpose] = transpose
  def S: TDSL[DepApp[NatKind]] = split(tileSize) //slide(3)(1)
  def J: TDSL[Join] = join
  def *(x: TDSL[Rise]): TDSL[App] = map(x)
  def **(x: TDSL[Rise]): TDSL[App] = map(map(x))
  def ***(x: TDSL[Rise]): TDSL[App] = map(map(map(x)))
  def ****(x: TDSL[Rise]): TDSL[App] = map(map(map(map(x))))
  def *****(x: TDSL[Rise]): TDSL[App] = map(map(map(map(map(x)))))
  def ******(x: TDSL[Rise]): TDSL[App] = map(map(map(map(map(map(x))))))

  def λ(f: TDSL[Identifier] => TDSL[Expr]): TDSL[Lambda] = fun(f)

  // map in LCNF
  def *!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(x, i)))
  }

  def **!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(*!(x), i)))
  }

  def ***!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(**!(x), i)))
  }

  def ****!(x: TDSL[Rise]): TDSL[App] = {
    val i = identifier(freshName("e"))
    map(lambda(i, app(***!(x), i)))
  }

  def testMultiple(list: List[Rise], gold: Rise): Unit = {
    assert(list.forall(betaEtaEquals(_, gold)))
  }
}
