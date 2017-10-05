import mergeSort.merge

object mergeSort {

  def main(args: Array[String]): Unit = {
    println("Merge sort ")
    val testList = List(2,3,5,1,5,7,2,3,6,4,6,89,22,33,4,13,52,45,11)
    println("Input List: "+ testList)
    println("sorted result of List(num1, num2,..) : "+ msort(testList))
    println("sorted w/ patter matching pair : "+ msort_pair(testList))
  } //end of main

  //  def merge(ints: List[Int], ints1: List[Int]): scala.List[Int] = ???
//  def merge(xs: List[Int], ys: List[Int]): scala.List[Int] = {
  def merge(xs: List[Int], ys: List[Int]): List[Int] = {
    xs match {
      case Nil =>
        ys
      case x :: xs1 =>
        ys match {
          case Nil =>
            xs
          case y :: ys1 =>
            if (x < y) x :: merge(xs1, ys)
            else y :: merge(xs, ys1)
        }
    }
  }//end of def merge
  def msort(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }//end of def msort
  /*rewritting using a pattern matching over pairs    */
  def merge_pair(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (x :: xs1, y :: ys1) =>
      if (x < y) x :: merge_pair(xs1, ys)
      else y :: merge_pair(xs, ys1)
  }

  def msort_pair(xs: List[Int]): List[Int] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (first, second) = xs splitAt n
      merge_pair(msort_pair(first), msort_pair(second))
    }
  }//end of def msort
}
