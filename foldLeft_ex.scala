object foldLeft_ex {
  def main(args: Array[String]) = {
    val list_example = List(1,2,3,4,5)
    val list_example_Double = List(1.0,2.0,3.0,4,5)
    println("sum1 by list.foldLest(0)((r,c) => r+c) : "+ sum1(list_example))
    println("sum2 by list.foldLest(0)(_+_) : "+ sum2(list_example))
    println("sum1 by list.foldLest(0)((r,c) => r+c) : "+ product1(list_example))
    println("sum2 by list.foldLest(0)(_+_) : "+ product2(list_example))
    println("count by list.foldLest(0)((r+_) => r + 1) : "+ product2(list_example))
    println("last element by last[A](list: List[A]): A = list.foldLeft(list.haed)((_, c) => c) : "+
    last(list_example))
    println("average by list.foldLeft(0.0)(_+_)/list.foldLest(0.0)((r+_) => r + 1) : "
      + average(list_example_Double))
  }
  def sum1(list: List[Int]): Int = list.foldLeft(0)((r, c) => r + c) //intial value is 0 for sum
  def product1(list: List[Int]): Int = list.foldLeft(1)((r, c) => r * c) //intial value is 1 for product
  def sum2(list: List[Int]): Int = list.foldLeft(0)(_+_)
  def product2(list: List[Int]): Int = list.foldLeft(1)(_*_)

  /*
  ****************************************************
   *  An empty list has 0 elements, so we use 0. We just want to increase the result value by one.
   *  We call that parameter ‘r’ in this solution.
   *  We just want to increase the result value by one.
   *  and We don’t care about the actual value of each list element,
   *  so we call the second parameter ‘_’, which means it should be discarded.
   * **************************************************
   * */
  def count(list: List[Any]): Int =
    list.foldLeft(0)((r,_) => r + 1)
  def average(list: List[Double]): Double = list.foldLeft(0.0)(_+_) / list.foldLeft(0.0)((r,_) => r+1)

/*
Given a List[A] return the last value in the list using type parameter 'A'
'A' allows us to take a list of any type of contents, and return a result of just that type
For each item in the list, it just returns that item itself.
So when it gets to the end of the list, the accumulator holds the last item.
We don’t use the accumulator value in the function literal, so it gets parameter name ‘_’.
 */
  def last[A](list: List[A]): A =
    list.foldLeft(list.head)((_, c) => c)
/*
 Takes a List[A] and returns the penultimate item (i.e. the next to last item) in the list).
  Instead of keeping just the current item it keeps a Pair containing the previous and current items.
  When foldLeft completes, its result is a Pair containing the next-to-last and last items.
   The “_1” method returns just the penultimate item.
 */
}
