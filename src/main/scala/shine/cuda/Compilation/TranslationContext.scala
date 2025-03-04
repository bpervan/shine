package shine.cuda.Compilation

import shine.DPIA.DSL._
import shine.DPIA.Phrases.Phrase
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, FragmentType, read}
import shine.DPIA.primitives.imperative.Assign
import shine.DPIA.{accT, expT}
import shine.cuda.primitives.imperative.ForFragmentElements

class TranslationContext() extends shine.OpenCL.Compilation.TranslationContext {
  override def assign(dt: DataType,
                      lhs: Phrase[AccType],
                      rhs: Phrase[ExpType]): Phrase[CommType] = {
    dt match {
      case f: FragmentType =>
        ForFragmentElements(f, rhs, lhs,
          λ(expT(dt, read))(x =>
            λ(accT(dt))(o =>
              Assign(dt, o, x))))

      case _ =>
        super.assign(dt, lhs, rhs)
    }
  }
}
