package ru.hse.aml.oop

class ListImpl[A]: //extends List[A]:
    def map[B](f: A => B): ListImpl[B] = ???
    def flatMap[B](k: A => ListImpl[B]): ListImpl[B] = ???
    def foreach(f: A => Unit): Unit = ???
    def withFilter(p: A => Boolean): ListImpl[A] = ???
