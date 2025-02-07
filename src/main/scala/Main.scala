import ru.hse.aml.FP.FPListImpl
import ru.hse.aml.FP.FList

@main def hello(): Unit = 
    
    var list = FList.Cons(1, FList.Cons(2, FList.Cons(3, FList.Empty)))

    println(list.at(0)) // Some(1)
    println(list.at(1)) // Some(2)
    println(list.at(2)) // Some(3)
    println(list.at(3)) // None

    println(list.delete(1)) // true, but nothing changes (very FP)
    println(list.at(0)) // Some(1)
    println(list.at(1)) // Some(2)
    println(list.at(2)) // Some(3)

    println(list.all(_ > 0)) // true
    println(list.all(_ > 1)) // false
    println(list.any(_ > 1)) // true
    println(list.any(_ > 3)) // false
    println(list.head) // Some(1)
    println(list.length) // 3