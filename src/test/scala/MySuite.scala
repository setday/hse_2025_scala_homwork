import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

import ru.hse.aml.oop.ListImpl

import ru.hse.aml.FP.FPListImplExtension
import ru.hse.aml.FP.FList

class ListImplSuit extends AnyFlatSpec, Matchers:
    "ListImpl" should "work correctly" in {
        val list = new ListImpl[Int]

        list.push(2)
        list.push(1)

        list.length must equal (2)
        list.at(0) must equal (Some(2))
        list.at(1) must equal (Some(1))

        list.delete(0) must be (true)
        list.length must equal (1)
        list.at(0) must equal (Some(1))

        list.push(3)
        list.all(_ > 0) must be (true)
        list.all(_ > 1) must be (false)
        list.any(_ > 1) must be (true)
        list.any(_ > 3) must be (false)
        list.head must equal (Some(1))
    }

    it should "support withFilter" in {
        val list = new ListImpl[Int]

        list.push(2)
        list.push(1)
        list.push(3)

        val filtered = list.withFilter(_ > 1)
        filtered.length must equal (2)
        filtered.at(0) must equal (Some(2))
        filtered.at(1) must equal (Some(3))
    }

    it should "support map" in {
        val list = new ListImpl[Int]

        list.push(2)
        list.push(1)
        list.push(3)

        val mapped = list.map(_ * 2)
        mapped.length must equal (3)
        mapped.at(0) must equal (Some(4))
        mapped.at(1) must equal (Some(2))
        mapped.at(2) must equal (Some(6))
    }

    it should "support flatMap" in {
        val list = new ListImpl[Int]

        list.push(1)
        list.push(3)

        val flatMapped = list.flatMap(x => {
            val res = new ListImpl[Int]
            res.push(x)
            res.push(x * 2)
            res
        })

        flatMapped.length must equal (4)
        flatMapped.at(0) must equal (Some(1))
        flatMapped.at(1) must equal (Some(2))
        flatMapped.at(2) must equal (Some(3))
        flatMapped.at(3) must equal (Some(6))
    }

    it should "support foreach" in {
        val list = new ListImpl[Int]

        list.push(1)
        list.push(3)

        var sum = 0
        list.foreach(x => sum += x)

        sum must equal (4)
    }

class FPListImplSuit extends AnyFlatSpec, Matchers:
    "FPListImpl" should "work correctly" in {
        var list = FList.Cons(1, FList.Cons(2, FList.Cons(3, FList.Empty)))

        list.at(0) must equal (Some(1))
        list.at(1) must equal (Some(2))
        list.at(2) must equal (Some(3))
        list.at(3) must equal (None)

        list.delete(1) must be (true)
        list.at(0) must equal (Some(1))
        list.at(1) must equal (Some(2))
        list.at(2) must equal (Some(3))
        list.delete(-1) must be (false)
        list.delete(3) must be (false)

        list.all(_ > 0) must be (true)
        list.all(_ > 1) must be (false)
        list.any(_ > 1) must be (true)
        list.any(_ > 3) must be (false)
        list.head must equal (Some(1))
        list.length must equal (3)
    }

    it should "support map" in {
        var list = FList.Cons(1, FList.Cons(2, FList.Cons(3, FList.Empty)))

        val mapped = list.map(_ * 2)
        mapped.at(0) must equal (Some(2))
        mapped.at(1) must equal (Some(4))
        mapped.at(2) must equal (Some(6))
        mapped.at(3) must equal (None)
    }

    it should "be iterable" in {
        var list = FList.Cons(1, FList.Cons(2, FList.Cons(3, FList.Empty)))

        var sum = 0
        for i <- list do
            sum += i

        sum must equal (6)
    }

    it should "support flatMap" in {
        var list = FList.Cons(1, FList.Cons(3, FList.Empty))
        var answerList = List(1, 2, 3, 6)

        val flatMapped = list.flatMap(x => FList.Cons(x, FList.Cons(x * 2, FList.Empty)))

        flatMapped.length must equal (answerList.length)
        for i <- 0 until answerList.length do
            flatMapped.at(i) must equal (Some(answerList(i)))
    }

class ListImplProperties extends Properties("ListImpl"):
    property("push") = forAll { (xs: List[Int]) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        list.length == xs.length && (0 until xs.length).forall(i => list.at(i) == Some(xs(i)))
    }

    property("delete") = forAll { (xs: List[Int], i: Int) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        val before = list.length
        val deleted = list.delete(i)
        val after = list.length
        if i < 0 || i >= xs.length then
            !deleted && before == after
        else
            deleted && before == after + 1 && (0 until i).forall(j => list.at(j) == Some(xs(j))) && (i until xs.length - 1).forall(j => list.at(j) == Some(xs(j + 1)))
    }

    property("all") = forAll { (xs: List[Int], p: Int => Boolean) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        list.all(p) == xs.forall(p)
    }

    property("any") = forAll { (xs: List[Int], p: Int => Boolean) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        list.any(p) == xs.exists(p)
    }

    property("head") = forAll { (xs: List[Int]) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        list.head == xs.headOption
    }

    property("withFilter") = forAll { (xs: List[Int], p: Int => Boolean) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        val filtered = list.withFilter(p)
        filtered.length == xs.count(p) && (0 until xs.length).filter(i => p(xs(i))).forall(i => filtered.at(i) == Some(xs(i)))
    }

    property("map") = forAll { (xs: List[Int], f: Int => Int) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        val mapped = list.map(f)
        mapped.length == xs.length && (0 until xs.length).forall(i => mapped.at(i) == Some(f(xs(i))))
    }

    property("flatMap") = forAll { (xs: List[Int], f: Int => List[Int]) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        val flatMapped = list.flatMap(x => {
            val res = new ListImpl[Int]
            f(x).foreach(res.push)
            res
        })
        flatMapped.length == xs.map(f).map(_.length).sum && (0 until xs.length).forall(i => f(xs(i)).forall(x => flatMapped.at(flatMapped.length - xs.map(f).map(_.length).sum + i) == Some(x)))
    }

    property("foreach") = forAll { (xs: List[Int]) =>
        val list = new ListImpl[Int]
        xs.foreach(list.push)
        var sum = 0
        list.foreach(sum += _)
        sum == xs.sum
    }

class FPListImplProperties extends Properties("FPListImpl"):
    property("at") = forAll { (xs: List[Int], i: Int) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        list.at(i) == xs.lift(i)
    }

    property("delete") = forAll { (xs: List[Int], i: Int) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        val before = list.length
        val deleted = list.delete(i)
        val after = list.length
        if i < 0 || i >= xs.length then
            !deleted && before == after
        else
            deleted && before == after + 1 && (0 until i).forall(j => list.at(j) == Some(xs(j))) && (i until xs.length - 1).forall(j => list.at(j) == Some(xs(j + 1)))
    }

    property("all") = forAll { (xs: List[Int], p: Int => Boolean) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        list.all(p) == xs.forall(p)
    }

    property("any") = forAll { (xs: List[Int], p: Int => Boolean) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        list.any(p) == xs.exists(p)
    }

    property("head") = forAll { (xs: List[Int]) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        list.head == xs.headOption
    }

    property("map") = forAll { (xs: List[Int], f: Int => Int) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        val mapped = list.map(f)
        mapped.length == xs.length && (0 until xs.length).forall(i => mapped.at(i) == Some(f(xs(i))))
    }

    property("foreach") = forAll { (xs: List[Int]) =>
        val list = xs.foldRight(FList.Empty: FList[Int])((x, xs) => FList.Cons(x, xs))
        var sum = 0
        list.foreach(sum += _)
        sum == xs.sum
    }
