package sturdy.values.adts

import sturdy.data.{MakeJoined, MayJoin, joinComputations}
import sturdy.effect.EffectStack
import sturdy.effect.environment.Environment
import sturdy.values.Top.top
import sturdy.values.{Join, Top, Topped}

final class ToppedADTConstructorOps[Name, V, VADTConstructor]
(using ops: ADTConstructorOps[Name, V, VADTConstructor]) extends ADTConstructorOps[Name, V, Topped[VADTConstructor]] {

  override def constructor(constructor: ADTConstructor[Name, V]): Topped[VADTConstructor] =
    Topped.Actual(ops.constructor(constructor))
}

final class ToppedADTMatching[Name, V, VADTConstructor, R]
(using ops: ADTMatching[Name, V, VADTConstructor, R])
(using EffectStack, Join[R], Top[V]) extends ADTMatching[Name, V, Topped[VADTConstructor], R] {

  override def matches[J[_] <: MayJoin[_]](scrutinee: Topped[VADTConstructor],
                                           matchedName: Name,
                                           matchedParameters: Seq[Name],
                                           environment: Environment[Name, V, J],
                                           ifMatches: => R,
                                           otherwise: => R): R =
    scrutinee match
      case Topped.Actual(actualScrutinee) =>
        ops.matches(actualScrutinee, matchedName, matchedParameters, environment, ifMatches, otherwise)
      case Topped.Top =>
        def evalMatchingBranch() = environment.scoped {
          for parameter <- matchedParameters do {
            environment.bind(parameter, top)
          }
          ifMatches
        }

        joinComputations(evalMatchingBranch())(otherwise)

  override def matchFailure(): R = ???
}

