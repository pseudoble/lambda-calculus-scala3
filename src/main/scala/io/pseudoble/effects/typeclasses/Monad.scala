package io.pseudoble.effects.typeclasses

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
  
  override def ap[A,B](ff: F[A => B])(fa: F[A]): F[B] =
    flatMap(ff) { f => 
      fmap(fa)(f) 
    }

  override def fmap[A,B](fa: F[A])(f: (A => B)): F[B] = 
    flatMap(fa)(f andThen pure)
}