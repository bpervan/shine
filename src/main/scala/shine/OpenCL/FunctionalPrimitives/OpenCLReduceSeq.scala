package shine.OpenCL.FunctionalPrimitives

import shine.DPIA.Compilation.TranslationToImperative._
import shine.DPIA.Compilation._
import shine.DPIA.DSL._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL.IntermediatePrimitives.OpenCLReduceSeqI
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class OpenCLReduceSeq(unroll: Boolean)
                                (val n: Nat,
                                 val initAddrSpace: AddressSpace,
                                 val dt1: DataType,
                                 val dt2: DataType,
                                 val f: Phrase[ExpType ->: ExpType ->: ExpType],
                                 val init: Phrase[ExpType],
                                 val array: Phrase[ExpType])
  extends ExpPrimitive
{
  f :: expT(dt2, read) ->: expT(dt1, read) ->: expT(dt2, read)
  init :: expT(dt2, read)
  array :: expT(n`.`dt1, read)
  override val t: ExpType = expT(dt2, read)

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    //TODO This is wrong!
    println("WARNING: opencl reduce seq acceptor translation is deprecated," +
      "implicit copies might happen")
    con(array)(λ(expT(n`.`dt1, read))(X =>
      OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt2, read))(x => λ(expT(dt1, read))(y => λ(accT(dt2))(o =>
          acc( f(x)(y) )( o )))),
        init, X, λ(expT(dt2, write))(r => acc(r)(A)), unroll)(context)))
  }

  override def continuationTranslation(C: Phrase[ExpType ->: CommType])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    //TODO same for ReduceSeq/AbstractReduce
    con(array)(λ(expT(n`.`dt1, read))(X =>
      OpenCLReduceSeqI(n, initAddrSpace, dt1, dt2,
        λ(expT(dt2, read))(x => λ(expT(dt1, read))(y => λ(accT(dt2))(o =>
          acc( f(x)(y) )( o )))),
        init, X, C, unroll)(context)))
  }
}
