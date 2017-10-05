//import mergeSort.merge_pair
import math.Ordering

object mergeSort2 {
  def main(args: Array[String]): Unit = {
    println("Merge sort ")
    val testList = List(2,3,5,1,5,7,2,3,6,4,6,89,22,33,4,13,52,45,11)
    println("Input List: "+ testList)
    println("sorted result of List(num1, num2,..) : "+ msort(testList)((x: Int, y: Int) => x < y))
    println("sorted result of List(num1, num2,..) w/o Input type : "+ msort(testList)((x, y) => x < y))  //w/o type same result
    println("sorted w/ patter matching pair : "+ msort_pair(testList)((x: Int, y: Int) => x < y))
    /* ord: Ordering               */
    println("sorted w/ patter matching pair_ord- Ordering : "+ msort_pair_ord(testList)(Ordering.Int))
    println("sorted w/ patter matching pair_ implicit ord- Ordering : "+ msort_pair_ord_implicit(testList))

    val testList2 = List("apple", "pineapple", "grapes", "banana")
    println("Input List: "+ testList2)
    println("sorted result of List(String1, String2,..) : "+
      msort(testList2)((x: String, y: String) => x.compareTo(y) < 0))
    println("sorted result of List(String1, String2,..) : "+
      msort(testList2)((x: String, y: String) => x.compareTo(y) < 0))
    println("sorted result of List(String1, String2,..)- ord: Ordering : "+
      msort_ord(testList2)(Ordering.String))
    println("sorted result of List(String1, String2,..)- implicit ord: Ordering : "+
      msort_ord_implicit(testList2))
    /* test x.compareTo(y)
    * : return minus if 1st string is less than 2nd string, 0 for same , plus for bigger*/
    println("x.compareTo(y) : " + "This is".compareTo("This are"))  // < : +
    println("x.compareTo(y) : " + "This is".compareTo("This is"))   //== : 0
    println("x.compareTo(y) : " + "That".compareTo("This"))         // > : -
  } //end of main

  //  def merge(ints: List[Int], ints1: List[Int]): scala.List[Int] = ???
  //  def merge(xs: List[Int], ys: List[Int]): scala.List[Int] = {



  //end of def merge
  /*rewritting using a pattern matching over pairs
   * the most fliexible design is to make the function sort polymorphic and to pass
    * the comparison operation, "(lt: (T,T))" as an additional paremeter*/


  def msort_pair[T](xs: List[T])(lt: (T, T) => Boolean) : List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge_pair(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          /* We can't assure the < is defined in the element of type T */
          if (lt(x, y)) x :: merge_pair(xs1, ys) //error : "<" is not a member of parameter T
          else y :: merge_pair(xs, ys1)
      }
      val (first, second) = xs splitAt n
      merge_pair(msort_pair(first)(lt), msort_pair(second)(lt))
    }
  }//end of def msort

  /*  instead of parameterizing with the "lt" operation directly,
        * we could parameterize with Ordering(scala.math.Ordering[T]) instead */
  def msort_pair_ord[T](xs: List[T])(ord: Ordering[T]) : List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge_pair_ord(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          /* We can't assure the < is defined in the element of type T */
          if (ord.lt(x, y)) x :: merge_pair_ord(xs1, ys) //error : "<" is not a member of parameter T
          else y :: merge_pair_ord(xs, ys1)
      }
      val (first, second) = xs splitAt n
      merge_pair_ord(msort_pair_ord(first)(ord), msort_pair_ord(second)(ord))
    }
  }//end of def msort_ord

  def msort_pair_ord_implicit[T](xs: List[T])(implicit ord: Ordering[T]) : List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge_pair_ord(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          /* We can't assure the < is defined in the element of type T */
          if (ord.lt(x, y)) x :: merge_pair_ord(xs1, ys) //error : "<" is not a member of parameter T
          else y :: merge_pair_ord(xs, ys1)
      }
      val (first, second) = xs splitAt n
      merge_pair_ord(msort_pair_ord_implicit(first), msort_pair_ord_implicit(second))
    }
  }//end of def msort_ord_implicit

  def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]) : List[T] = {
        xs match {
          case Nil =>
            ys
          case x :: xs1 =>
            ys match {
              case Nil =>
                xs
              case y :: ys1 =>
                if (lt(x, y)) x :: merge(xs1, ys)
                else y :: merge(xs, ys1)
            }
        }
      }
      val (first, second) = xs splitAt n
      merge(msort(first)(lt), msort(second)(lt))
    }
  }//end of def msort

  def msort_ord[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge_ord(xs: List[T], ys: List[T]) : List[T] = {
        xs match {
          case Nil =>
            ys
          case x :: xs1 =>
            ys match {
              case Nil =>
                xs
              case y :: ys1 =>
                if (ord.lt(x, y)) x :: merge_ord(xs1, ys)
                else y :: merge_ord(xs, ys1)
            }
        }
      }
      val (first, second) = xs splitAt n
      merge_ord(msort_ord(first)(ord), msort_ord(second)(ord))
    }
  }//end of def msort_ord

  /* passing around "lt", "ord" value is cumbersone, we can avoide this by making
     * ord an implict parameter */
  def msort_ord_implicit[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge_ord(xs: List[T], ys: List[T]) : List[T] = {
        xs match {
          case Nil =>
            ys
          case x :: xs1 =>
            ys match {
              case Nil =>
                xs
              case y :: ys1 =>
                if (ord.lt(x, y)) x :: merge_ord(xs1, ys)
                else y :: merge_ord(xs, ys1)
            }
        }
      }
      val (first, second) = xs splitAt n
      merge_ord(msort_ord_implicit(first), msort_ord_implicit(second))
    }
  }//end of def msort_ord_implict

}
