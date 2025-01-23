package ru.hse.aml.FP

trait FPList[F[_]]:
  extension [A](xs: F[A])
    def at(idx: Int): Option[A]
    def delete(idx: Int): Boolean

    def head: Option[A]
    def length: Int
  
    def all(p: A => Boolean): Boolean
    def any(p: A => Boolean): Boolean

enum FList[+A]:
    case Empty
    case Cons(x: A, xs: FList[A])