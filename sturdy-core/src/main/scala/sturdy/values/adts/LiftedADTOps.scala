package sturdy.values.adts

import sturdy.data.MayJoin
import sturdy.effect.environment.Environment

final class LiftedADTConstructorOps[Name, V, VADTConstructor]
(inject: VADTConstructor => V)
(using ops: ADTConstructorOps[Name, V, VADTConstructor]) extends ADTConstructorOps[Name, V, V] {
  override def constructor(constructor: ADTConstructor[Name, V]): V = inject(ops.constructor(constructor))
}

final class LiftedADTMatching[Name, V, VADTConstructor, R]
(extract: V => VADTConstructor)
(using ops: ADTMatching[Name, V, VADTConstructor, R]) extends ADTMatching[Name, V, V, R] {
  override def matches[J[_] <: MayJoin[_]](scrutinee: V,
                                           matchedName: Name,
                                           matchedParameters: Seq[Name],
                                           environment: Environment[Name, V, J],
                                           ifMatches: => R,
                                           otherwise: => R): R =
    ops.matches(extract(scrutinee), matchedName, matchedParameters, environment, ifMatches, otherwise)

  override def matchFailure(): R = ops.matchFailure()
}