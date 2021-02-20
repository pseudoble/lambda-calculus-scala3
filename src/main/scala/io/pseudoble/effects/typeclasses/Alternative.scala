package io.pseudoble.effects.typeclasses

import scala.annotation.targetName
import io.pseudoble.effects.typeclasses.Applicative

/** Alternative Type Class */
trait Alternative[F[_]] extends Applicative[F]:
  /** empty - A container that may not have a value must have an empty state. */
  def empty[A]: F[A]
  
  extension [A] (fa: F[A])
    /** alternative / orElse - Try the first container. If it's empty, return the second container instead. */
    infix def orElse(other: => F[A]): F[A]
    @targetName("alternative") def <|>(other: => F[A]) = fa orElse other
