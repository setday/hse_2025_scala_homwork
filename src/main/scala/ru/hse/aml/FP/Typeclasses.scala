package ru.hse.aml.FP

trait Functor[F[_]]:
  extension [A](xs : F[A]) 
    def map[B](f : A => B) : F[B]

trait Monad[F[_]] extends Functor[F]:
  def pure[A](x : A): F[A]

  extension [A](m : F[A])
    def flatMap[B](k : A => F[B]) : F [B]
    def >>=[B](k: A => F[B]) : F[B] = m.flatMap(k)

trait Forable[F[_]] extends Functor[F]:
  extension [A](xs : F[A])
    def foreach(f : A => Unit) : Unit
