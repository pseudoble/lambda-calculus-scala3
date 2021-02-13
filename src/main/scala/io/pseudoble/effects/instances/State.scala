package io.pseudoble.effects.instances

import io.pseudoble.effects.typeclasses.Monad


/** A type instance for the State Monad. */
class State[S, +A](private val f: S => (S, A)) {
  def run(state: S): (S,A) = f(state)
  def runV(state: S): A = f(state)._2
  def runS(state: S): S = f(state)._1
}
object State:
  type StateInstance[S] = [a] =>> State[S, a]
  given [S]: Monad[StateInstance[S]] with {
    def pure[A](a: => A): State[S, A] = State { s => (s, a) }
    extension [A, B](fa: State[S, A])
      def flatMap(f: => A => State[S, B]): State[S, B] =
        State { s =>
          fa.f(s) match {
            case (s2, a) => f(a).f(s2)
          }
        }
  }