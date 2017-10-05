val numberList1 = List(4,3,1,5,9,10,6,21,7)
val numberList2 = List(22,33,44)
numberList1.length/2
numberList1.length
numberList1.last
println("all except last : "+numberList1.init)
val xs_take_n = numberList1 take 3
println("xs takes n : "+ xs_take_n )
val xs_drop_n = numberList1 drop 3
println("xs drop n : "+ xs_drop_n)
numberList1(0)
numberList1(1)
numberList1(3)
println("**************")
val newNum =  numberList1 ++ numberList2
println("numberList1 ++ numberList2 : "+ newNum)
def reverse_test[T](xs: List[T]): List[T] = xs match {
  case List() => xs
    //:: constructor(construct head, tail) , ++ concatenator(merge two lists)
  case y :: ys => reverse_test(ys) ++ List(y)
}
val num_take = numberList1 take 2
println("num_take :" + num_take)
val num_drop = numberList1 drop 2
println("num_drop :" + num_drop)

