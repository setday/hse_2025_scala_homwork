import ru.hse.aml.FP.FPListImplExtension
import ru.hse.aml.FP.FList

@main def hello(): Unit = 
    
    var list = FList.Cons(5, FList.Cons(2, FList.Cons(3, FList.Empty)))

    for i <- list.flatMap(x => FList.Cons(x, FList.Cons(x * 2, FList.Empty))) do
        println(i)
