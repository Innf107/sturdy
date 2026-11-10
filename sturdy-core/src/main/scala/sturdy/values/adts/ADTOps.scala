package sturdy.values.adts

import sturdy.data.MayJoin
import sturdy.effect.environment.Environment
import sturdy.effect.failure.Failure

case class ADTConstructor[Name, +V](name: Name, arguments: Seq[V])

trait ADTConstructorOps[Name, V, VADTConstructor] {
  def constructor(constructor: ADTConstructor[Name, V]): VADTConstructor
}

trait ADTMatching[Name, V, VADTConstructor, R] {
  // All parameters should be bound in the environment before evaluating ifMatches
  def matches[J[_] <: MayJoin[_]](scrutinee: VADTConstructor,
                                  matchedName: Name,
                                  matchedParameters: Seq[Name],
                                  environment: Environment[Name, V, J],
                                  ifMatches: => R,
                                  otherwise: => R): R

  def matchFailure(): R
}

given ConcreteADTConstructorOps[Name, V]: ADTConstructorOps[Name, V, ADTConstructor[Name, V]] with
  override def constructor(constructor: ADTConstructor[Name, V]): ADTConstructor[Name, V] =
    constructor

given ConcreteADTMatching[V, R](using Failure): ADTMatching[String, V, ADTConstructor[String, V], R] with
  override def matches[J[_] <: MayJoin[_]](scrutinee: ADTConstructor[String, V],
                                           matchedName: String,
                                           matchedParameters: Seq[String],
                                           environment: Environment[String, V, J],
                                           ifMatches: => R,
                                           otherwise: => R): R = {
    val ADTConstructor(scrutineeName, scrutineeArguments) = scrutinee
    if scrutineeName == matchedName then
      environment.scoped {
        for (parameter, boundValue) <- matchedParameters.zip(scrutineeArguments) do
          environment.bind(parameter, boundValue)
        ifMatches
      }
    else
      otherwise
  }

  override def matchFailure(): R = ???
