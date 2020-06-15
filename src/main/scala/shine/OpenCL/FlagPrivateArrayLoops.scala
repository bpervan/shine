package shine.OpenCL

import shine.DPIA.FunctionalPrimitives.Idx
import shine.DPIA.ImperativePrimitives.{For, ForNat, IdxAcc}
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics.ArrayData
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.Primitives.OpenCL
import shine.OpenCL.Primitives.OpenCL._

import scala.collection.mutable

object FlagPrivateArrayLoops {
  def apply(p: Phrase[CommType]): Phrase[CommType] = {
    val vs = varsToEliminate(p)
    val p2 = eliminateLoopVars(p, vs)
    if (vs.nonEmpty) {
      println(s"WARNING: could not eliminate variables $vs")
    }
    p2
  }

  private def varsToEliminate(p: Phrase[CommType]): mutable.Set[String] = {
    val eliminateVars = mutable.Set[String]()

    case class Visitor(privMemIdents: Set[Identifier[_]],
                       indexingIdents: Set[String])
      extends VisitAndRebuild.Visitor
    {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case OpenCL.New(AddressSpace.Private, _, Lambda(i: Identifier[_], _)) =>
          Continue(p, this.copy(privMemIdents = privMemIdents + i))
        case Idx(_, _, i, _) =>
          Continue(p, this.copy(indexingIdents = indexingIdents ++ collectIndexingIdents(i)))
        case IdxAcc(_, _, i, _) =>
          Continue(p, this.copy(indexingIdents = indexingIdents ++ collectIndexingIdents(i)))
        case i: Identifier[_] if privMemIdents(i) =>
          eliminateVars ++= indexingIdents
          Stop(p)
        case Literal(ArrayData(_)) =>
          eliminateVars ++= indexingIdents
          Stop(p)
        case pf: OpenCL.ParFor => pf.loopBody match {
          case Lambda(i: Identifier[_], Lambda(o: Identifier[_], _))
            if collectIdents(pf.out).exists(privMemIdents(_)) =>
              eliminateVars += i.name
              Continue(p, this.copy(privMemIdents = privMemIdents + o))
          case _ => Continue(p, this)
        }
        case _ =>
          Continue(p, this)
      }
    }

    VisitAndRebuild(p, Visitor(Set(), Set()))
    eliminateVars
  }

  private def eliminateLoopVars(p: Phrase[CommType],
                                eliminateVars: mutable.Set[String]): Phrase[CommType] = {
    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = p match {
        case For(n, body @ Lambda(i: Identifier[_], _), _) if eliminateVars(i.name) =>
          eliminateVars -= i.name
          Continue(For(n, body, unroll = true), this)
        case ForNat(n, body @ DepLambda(i: NatIdentifier, _), _) if eliminateVars(i.name) =>
          eliminateVars -= i.name
          Continue(ForNat(n, body, unroll = true), this)
        case pf: OpenCL.ParFor => pf.loopBody match {
          case Lambda(i: Identifier[_], _) if eliminateVars(i.name) =>
            eliminateVars -= i.name
            Continue(ParFor(pf.level, pf.dim, unroll = true)
                           (pf.n, pf.dt, pf.out, pf.loopBody), this)
          case _ => Continue(p, this)
        }
        case pf: OpenCL.ParForNat => pf.loopBody match {
          case DepLambda(i: NatIdentifier, _) if eliminateVars(i.name) =>
            eliminateVars -= i.name
            Continue(ParForNat(pf.level, pf.dim, unroll = true)
                              (pf.n, pf.ft, pf.out, pf.loopBody), this)
          case _ => Continue(p, this)
        }
        case _ => Continue(p, this)
      }
    })
  }

  private def collectIndexingIdents[T <: PhraseType](p: Phrase[T]): Set[String] = {
    val idents = mutable.Set[String]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def nat[N <: Nat](n: N): N = {
        idents ++= n.varList.map(_.name)
        n
      }

      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_] => idents += i.name
          case _ =>
        }
        Continue(p, this)
      }
    })

    idents.toSet
  }

  private def collectIdents[T <: PhraseType](p: Phrase[T]): Set[Identifier[_]] = {
    val idents = mutable.Set[Identifier[_]]()

    VisitAndRebuild(p, new VisitAndRebuild.Visitor {
      override def phrase[T2 <: PhraseType](p: Phrase[T2]): Result[Phrase[T2]] = {
        p match {
          case i: Identifier[_]  => idents += i
          case _ =>
        }
        Continue(p, this)
      }
    })

    idents.toSet
  }
}
