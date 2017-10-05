object apply_func_to_elem_list {
  def main(args: Array[String]): Unit = {
    val list1 = List(2.0, 3.0, 6.0, 1.0, 5.0) //List[Double]
    val factor = 2
    println("scaleList(list1, factor) : " + scaleList(list1, factor))
    println("scaleList_map(list1, factor) : " + scaleList_map(list1, factor))

    val list2 = List(3, -2, 8, 6, -1, 5)
    println("squareList : " + squareList(list2))
    println("squareList_map : " + squareList_map(list2))
    val list2_str = List("apple", "pineapple", "orange", "banana")
    val list2_filter = list2 filter (x => x > 0)
//    println( "* list2 filter(x => x > 0) : " + list2 filter (x => x > 0))  //wrong result print all List(not filtered)
    println( "* list2 filter(x => x > 0) : " + list2_filter)
    val list2_filterNot = list2 filterNot (x => x > 0)
    println( "** list2 filterNot(x => x > 0) : " + list2_filterNot)
    val list2_partition = list2 partition (x => x > 0)
    println( "*** list2 partition (x => x > 0) : " + list2_partition) //pair of (filter, filterNot)
    val list2_takeWhile = list2 takeWhile (x => x > 0)
    println( "# list2 takeWhile (x => x > 0) : " + list2_takeWhile) // 3(true) but -2(false)-so stop, so just 3
    val list2_dropWhile = list2 dropWhile (x => x > 0)
    println( "## list2 dropWhile (x => x > 0) : " + list2_dropWhile) // remainder of takeWhile
    val list2_span = list2 span (x => x > 0)
    println( "### list2 span (x => x > 0) : " + list2_span) // combination of takeWhile, dropWhile

    val pack_list = List("a","a","a","b","c","c","a")
    println("^ pack : "+ pack(pack_list))
    println("encode : "+ encode(pack_list))
/* List(List("a","a","a"), List("b") List("c","c") List("a"))       */
    def pack[T](xs: List[T]): List[List[T]] = xs match {
      case Nil => Nil
      case x :: xs1 =>
        val (first, rest) = xs span (y => y == x)//first is List with y == x, second is the rest
        first :: pack(rest)
    }
    /*pack with occurrence number
* List(("a",3), ("b",1), ("c",2), ("a",1)) */
    /* pack & simple tranformation by map */
    def encode[T](xs: List[T]): List[(T, Int)] =
    pack(xs) map (ys => (ys.head, ys.length))  // input is pack(xs) -> transform to pair(ys.head, ys.length)
    val list3 = List(1, -2, 3, 6, -1, 5)
    println("sum of list : "+ sumM(list3))
    println("product of list : "+ productM(list3))
    println("sumRF of list : "+ sumRL(list3))
    println("productRF of list : "+ productRL(list3))

    println("concate two Lists(foldRight) : " + concat(list2, list3))

    val numberList1 = List(4,3,1,5,9,10,6,21,7)
    val numberList2 = List(95, 91, 93)
    println("numberList1.last : " + numberList1.last)
    println("numberList1.last_last2 : " + last2(numberList1))
    println("all except last : "+numberList1.init)
    println("all except last_init2 : "+init2(numberList1))

    val xs_take_n = numberList1 take 3
    println("xs takes n : "+ xs_take_n )
    val xs_drop_n = numberList1 drop 3
    println("xs drop n : "+ xs_drop_n)
    val concatedNum = numberList1 ++ numberList2
    println("numberList1 ++ numberList2 : " + concatedNum)
    val reverseNum = numberList1.reverse
    println("numberList1.reverse: " + reverseNum)
    val updatedNum = numberList1 updated (2, 777)  //mutable tactic, index to 777
    println("* numberList1 updated (2, 1) : " + updatedNum)  //updated (n , x) : all with updated index n, it's value x
    val removeAtNum = removeAt(3, numberList1)  //index[3], which 4th elem, 5 removed
    println("^ removeAt(3, xs) : "+ removeAtNum)
    val takeNum = numberList1 take 3
    //all element befor 4th elem, so elems of index0,index1,index2
    println("numberList1 take 3 : "+ takeNum )  //List(4, 3, 1)
    val dropNum3 = numberList1 drop (3)
    println("NumberList1 drop 3 : "+ dropNum3)
    val dropNum4 = numberList1 drop (4)
    println("NumberList1 drop 4 : "+ dropNum4)
  }//end of main


  //end of main
  /* transform each element of list and then return the list of results.
  => multiply each element of a list by the same faxtor,  */
  def scaleList(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => xs
    case y :: ys => y * factor :: scaleList(ys, factor)
  }

  /* this scheme ca be generalized to methond map of the List calss. */
  //  abstract class List[T] {
  //      ...
  //   def map[U](f: T => U): List[U] = this match {
  //      case Nil => this
  //      case x :: xs => f(x) :: xs.map(f)
  //   }
//   def filter(p: T => Boolean): List[T] = this match {
//      case Nil => this
//      case x :: xs => if (p(x)) x :: xs.filter(p) else xs.filter(p)
//    }
//  }
  def scaleList_map(xs: List[Double], factor: Double) =
     xs.map (x => x * factor)

  def squareList(xs:List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => y * y :: squareList(ys)
  }
  def squareList_map(xs: List[Int]): List[Int] =
    xs map (x => x * x)

  def posElems_match(xs: List[Int]): List[Int] = xs match {
//    case Nil = Nil  /Check this and below , what is difference ?
    case Nil => xs
    case y :: ys => if(y > 0) y :: posElems_match(ys) else posElems_match(ys)
    }
  def posElems(xs: List[Int]): List[Int] =
    xs.filter (x => x > 0)
  /*xs filterNot p  - x => !p(x)         */

  /*Combine the elements of a list using a given operator. */
  def sumM(xs : List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + sumM(ys)
  }
  def productM(xs: List[Int]): Int = xs match {
    case Nil => 1
    case y :: ys => y * productM(ys)
  }

  /*List(x1, x2,.. xn) reduceLeft op = (..(x1 op x2) op ...) op xn  */
  //(x + y) becomes left argument x to add y, then go on and on until the end of List element.
  def sumRL(xs: List[Int]) = (0 :: xs) reduceLeft((x, y) => x + y)  //reduceLeft(_ + _)
  def productRL(xs: List[Int]) = (1 :: xs) reduceLeft((x, y)=> x * y) //reduceLeft(_ * _)
  /* ((x, y) = x + y)  same as (_ + _)  */
  def sumRL_(xs: List[Int]) = (0 :: xs) reduceLeft(_ + _)  //reduceLeft(_ + _)
  def productRL_(xs: List[Int]) = (1 :: xs) reduceLeft(_ * _) //reduceLeft(_ * _)

  /* foldLeft same as reduceLeft but there is z for accumulator such as 0 for sum, 1 for product  */
  /*(List(x1, x2,.. xn) foldLeft z) op = (..(z op x1) op ...) op xn  */
  def sumFL_(xs: List[Int]) = (xs foldLeft 0) (_ + _)  //(xs foldLeft 0) for sum
  def productFL_(xs: List[Int]) = (xs foldLeft 1) (_ * _) //(xs foldLeft 1) for product

  def concat[T](xs: List[T], ys: List[T]): List[T] =
    (xs foldRight ys)(_::_)
  /*but foldLeft type error (:: is not a member of type paremet T)  */
//  def concatFL[T](xs: List[T], ys: List[T]): List[T] =
//    (xs foldLeft ys)(_::_)
//  def last[T](xs: List[T]): List[T] = xs match {  //return value shoud be element not List[T]
  def last2[T](xs: List[T]): T = xs match {  //return haed not tail, so T
  case List() => throw new Error("last of empty list")
  case List(x) => x  //element as last one.
  case y :: ys => last2(ys)  //if it consists of head(y) and tail(ys), call last function with tail(ys)
  }
  /*complement set of last   */
  def init2[T](xs: List[T]): List[T] = xs match {  //return tail not head, so List[T]
      /* length : 0  */
    case List() => throw new Error("linit of empty list")
      /* length : 1   */
    case List(x) => List()  //what happened when List consists of one element, in this case : empty list, List()
      /* length : 2 or more */
    case y :: ys => y :: init2(ys) //what happended when list consists of one element followed by list
  }
  /*complexity is |xs|, input List size   */
  def concat2[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat2(zs, ys)
  }

  /* complexity : n( for concate ) * n( for reverse) : n is size of sucessor ? */
  def reverse2[T](xs: List[T]): List[T] = xs match {
//    case List() => xs  //when empty list, return itself(empty)
    case List() => Nil
      //::, constructor(construct head, tail from list)
    case y :: ys => reverse2(ys)++List(y) //++ concatenate op(merge two lists), so List(y) making list from head(elem)
  }
  /* remove element at index n from list, and then show all lists  */
/* List_name take n -> take first n element from List, elements of index0, index1, index2
  * List_name drop n -> remainer of "List_name take n" */
  def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
}//end of object apply_func_to_elem_list

