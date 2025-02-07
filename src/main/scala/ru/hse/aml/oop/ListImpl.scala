package ru.hse.aml.oop

class ListImpl[A] extends List[A]:
    
    private class Node(val value: A, var next: Option[Node])

    private var headNode: Option[Node] = None
    private var tailNode: Option[Node] = None
    private var listLength: Int = 0

    override def at(idx: Int): Option[A] = {
        if (idx < 0 || idx >= listLength) {
            return None
        }

        var current = headNode
        for (_ <- 0 until idx) {
            current = current.get.next
        }

        current.map(_.value)
    }
    override def delete(idx: Int): Boolean = {
        if (idx < 0 || idx >= listLength) {
            return false
        }

        if (idx == 0) {
            headNode = headNode.get.next
            listLength -= 1
            return true
        }

        var current = headNode
        for (_ <- 0 until idx - 1) {
            current = current.get.next
        }

        current.get.next = current.get.next.get.next
        listLength -= 1
        true
    }
    override def push(x: A): Unit = {
        val newNode = new Node(x, None)
        if (headNode.isEmpty) {
            headNode = Some(newNode)
            tailNode = Some(newNode)
        } else {
            tailNode.get.next = Some(newNode)
            tailNode = Some(newNode)
        }

        listLength += 1
    }

    override def head: Option[A] = headNode.map(_.value)
    override def length: Int = listLength

    override def all(p: A => Boolean): Boolean = {
        var current = headNode
        while (current.isDefined) {
            if (!p(current.get.value)) {
                return false
            }
            current = current.get.next
        }

        true
    }
    override def any(p: A => Boolean): Boolean = !all(x => !p(x))

    def map[B](f: A => B): ListImpl[B] = {
        val newList = new ListImpl[B]()
        var current = headNode
        while (current.isDefined) {
            newList.push(f(current.get.value))
            current = current.get.next
        }

        newList
    }
    def flatMap[B](k: A => ListImpl[B]): ListImpl[B] = {
        val newList = new ListImpl[B]()
        var current = headNode
        while (current.isDefined) {
            val list = k(current.get.value)
            list.foreach(newList.push)
            current = current.get.next
        }

        newList
    }
    def foreach(f: A => Unit): Unit = {
        var current = headNode
        while (current.isDefined) {
            f(current.get.value)
            current = current.get.next
        }
    }
    def withFilter(p: A => Boolean): ListImpl[A] = {
        val newList = new ListImpl[A]()
        var current = headNode
        while (current.isDefined) {
            if (p(current.get.value)) {
                newList.push(current.get.value)
            }
            current = current.get.next
        }

        newList
    }
