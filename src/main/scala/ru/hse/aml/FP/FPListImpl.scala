package ru.hse.aml.FP

given FPListImpl: FPList[FList] with
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
