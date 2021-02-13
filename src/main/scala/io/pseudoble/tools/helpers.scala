package io.pseudoble.tools

// pattern matching a string like a list
object scons {
  def apply(h: Char, t: String): String = h +: t
  def unapply(s: String): Option[(Char, String)] = s.headOption.map{ (_, s.tail) }
}

/** Pipe operator */
extension [A,B](left: => A) def |>(right: A => B): B = right(left)
