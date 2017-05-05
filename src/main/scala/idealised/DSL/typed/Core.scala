package idealised.DSL.typed

import idealised.Core._
import apart.arithmetic.NamedVar

object identifier {
  def apply[T <: PhraseType](name: String, t: T) = IdentPhrase(name, t)
}

trait funDef {

  def apply[T1 <: PhraseType, T2 <: PhraseType](t: T1)
                                               (f: IdentPhrase[T1] => Phrase[T2]): LambdaPhrase[T1, T2] = {
    val param = identifier(newName(), t)
    LambdaPhrase(param, f(param))
  }

}

object fun extends funDef

object \ extends funDef

object λ extends funDef

trait dependentFunDef {

  def apply[T <: PhraseType](f: NamedVar => Phrase[T]) = {
    val x = NamedVar(newName())
    NatDependentLambdaPhrase(x, f(x))
  }

  def apply[T <: PhraseType](f: NamedVar => Phrase[T], range: apart.arithmetic.Range) = {
    val x = NamedVar(newName(), range)
    NatDependentLambdaPhrase(x, f(x))
  }

  def apply[T <: PhraseType](f: DataTypeIdentifier => Phrase[T]) = {
    val x = DataTypeIdentifier(newName())
    TypeDependentLambdaPhrase(x, f(x))
  }

}

object _Λ_ extends dependentFunDef

object π1 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj1Phrase(pair)
}

object π2 {
  def apply[T1 <: PhraseType, T2 <: PhraseType](pair: Phrase[T1 x T2]) =
    Proj2Phrase(pair)
}