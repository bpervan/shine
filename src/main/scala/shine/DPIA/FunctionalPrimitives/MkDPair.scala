package shine.DPIA.FunctionalPrimitives


import shine.DPIA.Compilation.{TranslationContext, TranslationToImperative}
import shine.DPIA.DSL._
import shine.DPIA.ImperativePrimitives.{MkDPairFstI, MkDPairSndAcc, Skip}
import shine.DPIA.Phrases.VisitAndRebuild.KindVisitable
import shine.DPIA.Phrases._
import shine.DPIA.Semantics.OperationalSemantics
import shine.DPIA.Semantics.OperationalSemantics.{IndexData, Store, U32Data}
import shine.DPIA.Types._
import shine.DPIA.{ImperativePrimitives, _}

import scala.xml.Elem

final case class Filter(a: AccessType, n: Nat, elemT: DataType, f: Phrase[ExpType ->: ExpType], input:Phrase[ExpType])
  extends ExpPrimitive {
  private val binder: NatIdentifier = NatIdentifier(freshName("n"), arithexpr.arithmetic.GoesToRange(n))
  private val sndT = ArrayType(binder,  IndexType(n))
  override val t = expT(DepPairType[NatKind](binder, sndT), a)
  // Filter needs to allocate more memory than it's 'logical' type says

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
      `new`(DepPairType[NatKind](binder, sndT), output => {
        // We just newed it, so we know output is an identifier. We will need to play some tricks
        // here, and change it's type.
        acceptorTranslation(output.wr) `;` C(output.rd)
      })
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    con(input)(λ(expT(ArrayType(n, elemT), read))(input => {
      `new`(u32, counter => {
        val fst = NatIdentifier(counter.asInstanceOf[Identifier[_]].name)
        `for`(n, idx => {
          `new`(bool, testLocal => {
            acc(f(input `@` idx))(testLocal.wr) `;`
            IfThenElse(testLocal.rd,
            {
              ((MkDPairSndAcc[NatKind](fst, sndT, A) `@` fst) :=| IndexType(n)| idx) `;`
                (counter.wr :=| u32 | counter.rd + Literal(U32Data(1)))
            },
            Skip()
          ) `;` MkDPairFstI[NatKind](fst, A)
          })
        })
      })
    }
    ))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"filter"

  override def xmlPrinter: Elem = <filter></filter>

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = Filter(
    v.access(a),
    v.nat(n),
    v.data(elemT),
    VisitAndRebuild(f, v),
    VisitAndRebuild(input, v)
  )
}

final case class MkDPair[K <: Kind: KindName: KindReified:KindVisitable](a: AccessType, fst: K#I, sndT: DataType, snd: Phrase[ExpType])
  extends ExpPrimitive {
  override val t: ExpType = expT(DepPairType[K](fst, sndT), a)

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // Allocate for the resulting dependent pair,
    // then imperatively write the first element,
    // acc-translate and write the second element
    // and call the continuation on the result
    // TODO(federico) - This is allocating eagerly. Make it allocate lazily by adding a suitable primitive:
    //  ideally Dmatch(..,..., MkDPair(x, y))
    // should not allocate
    con(snd)(λ(expT(sndT, read))(snd => `new`(t.dataType, outVar => {
      MkDPairFstI(fst, outVar.wr) `;`
        acc(snd)(MkDPairSndAcc(fst, sndT, outVar.wr)) `;`
        C(outVar.rd)
    })))
  }

  override def acceptorTranslation(A: Phrase[AccType])(implicit context: TranslationContext): Phrase[CommType] = {
    import TranslationToImperative._
    // We have the acceptor already, so simply write the first element and then
    // the second element in sequentially
    ImperativePrimitives.MkDPairFstI(fst, A) `;`
      acc(snd)(ImperativePrimitives.MkDPairSndAcc(fst, sndT, A))
  }

  override def eval(s: Store): OperationalSemantics.Data = ???

  override def prettyPrint: String = s"${this.getClass.getSimpleName} (${fst}) (${PrettyPhrasePrinter(snd)})"

  override def xmlPrinter: Elem = <MkDPair a={ToString(a)} sndT={ToString(sndT)}>
    <fst>
      {ToString(fst)}
    </fst>
    <snd type={ToString(sndT)}>
      {Phrases.xmlPrinter(snd)}
    </snd>
  </MkDPair>.copy(label = {
    val name = this.getClass.getSimpleName
    s"${Character.toLowerCase(name.charAt(0))}${name.substring(1)}"
  })

  override def visitAndRebuild(v: VisitAndRebuild.Visitor): Phrase[ExpType] = MkDPair(
    v.access(a),
    implicitly[KindVisitable[K]].visit(v, fst),
    v.data(sndT),
    VisitAndRebuild(snd, v),
  )
}