package ru.hse.aml.FP

class FPListImpl extends FPList[FList], Forable[FList], Monad[FList]:

    extension [A](xs: FList[A]) override def at(idx: Int): Option[A] = {
        if (idx < 0) None
        else if (idx == 0) xs match {
            case FList.Cons(x, tail) => Some(x)
            case FList.Empty => None
        }
        else xs match {
            case FList.Cons(x, tail) => tail.at(idx - 1)
            case FList.Empty => None
        }
    }
    extension [A](xs: FList[A]) override def delete(idx: Int): Boolean = {
        if (idx < 0) false
        else if (idx == 0) xs match {
            case FList.Cons(x, tail) => true // No Side Effects =}
            case FList.Empty => false
        }
        else xs match {
            case FList.Cons(x, tail) => tail.delete(idx - 1)
            case FList.Empty => false
        }
    }

    extension [A](xs: FList[A]) override def head: Option[A] = xs match {
        case FList.Empty => None
        case FList.Cons(x, tail) => Some(x)
    }
    extension [A](xs: FList[A]) override def length: Int = xs match {
        case FList.Empty => 0
        case FList.Cons(x, tail) => 1 + tail.length
    }

    extension [A](xs: FList[A]) override def all(p: A => Boolean): Boolean = xs match {
        case FList.Empty => true
        case FList.Cons(x, tail) => p(x) && tail.all(p)
    }
    extension [A](xs: FList[A]) override def any(p: A => Boolean): Boolean = xs match {
        case FList.Empty => false
        case FList.Cons(x, tail) => p(x) || tail.any(p)
    }

    extension [A](xs : FList[A]) override def map[B](f : A => B) : FList[B] = xs match {
        case FList.Empty => FList.Empty
        case FList.Cons(x, tail) => FList.Cons(f(x), tail.map(f))
    }

    extension [A](xs : FList[A]) override def foreach(f : A => Unit) : Unit = xs match {
        case FList.Empty => ()
        case FList.Cons(x, tail) => f(x); tail.foreach(f)
    }

    override def pure[A](x: A): FList[A] = FList.Cons(x, FList.Empty)

    // Extra method to make flatMap work more elegant way
    extension [A](m: FList[A]) def |(n: FList[A]): FList[A] = m match {
        case FList.Empty => n
        case FList.Cons(x, tail) => FList.Cons(x, tail | n)
    }

    extension [A](m: FList[A]) override def flatMap[B](k: A => FList[B]): FList[B] = m match {
        case FList.Empty => FList.Empty
        case FList.Cons(x, tail) => k(x) match {
            case FList.Empty => tail.flatMap(k)
            case FList.Cons(y, ys) => FList.Cons(y, ys | tail.flatMap(k))
        }
    }

given FPListImplExtension: FPListImpl()
