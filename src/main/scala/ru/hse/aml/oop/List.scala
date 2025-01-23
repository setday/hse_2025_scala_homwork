package ru.hse.aml.oop

trait List[A]:
    def at(idx: Int): Option[A]
    def delete(idx: Int): Boolean
    def push(x: A): Unit

    def head: Option[A]
    def length: Int
  
    def all(p: A => Boolean): Boolean
    def any(p: A => Boolean): Boolean
