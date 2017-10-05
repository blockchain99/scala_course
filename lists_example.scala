package scala
object lists_example {
  def main(args: Array[String]): Unit = {
    val fruit: List[String] = List("apple", "orange", "pears")
    val nums: List[Int] = List(1,2,3,4)
    val diag3: List[List[Int]] = List(List(1,0,0), List(0,1,0), List(0,0,1))
    val empty: List[Nothing] = List()
/* :: construction list,  x :: xs means consruct new list with first element "x"
  * and followed by the elements of "xs"
  * new Cons(x, xs) == x :: xs*/
    val fruit2 = "apple" :: ("orange" :: ("pears" :: Nil))
    /* ":" left associate  */
    val nums2 = 1 :: (2 :: (3 :: (4 :: Nil)))
    val nums3 = 1 :: 2 :: 3 :: 4 :: Nil  // 1 ::( 2 ::( 3 :: (4 :: Nil)))
    val empty2 = Nil
    printList(fruit)
    println("***********")
    printList2(nums)
    println("***********")
    printList2(nums2)
    println("***********")
    println(" * isort(List(7,3,9,2)) : " + isort(List(7,3,9,2)))
  }//end of main



  /*print each element in List */
  def printList(args: List[_]) : Unit = {
    args.foreach(println)
  }
  def printList2(args: TraversableOnce[_]) : Unit = {
    args.foreach(println)
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)  // if tail(xs) match List() i.e, nil, just insert x(head) in nil List
      /* if tail(xs) match head(y)::tail(ys) format */
    case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
      /* if given x is equal or smaller than head(y) then given x :: (y ::ys)
         if given  x is larger than head(y) then head(y) :: insert(x, tail(ys)) */
  }

  /*insertion sort a list of number in ascdeing order
    * List(7,3,9,2)
    * 1. sort the tail List(3,9,2) to obtain(2,3,9)
    * 2. insert the head 7 in the right place to botain the result List(2,3,7,9)
    * (all precedent elements are smaller than or equal the inserted element &
    * all following elements are bigger than or equal the inserted element)
    * */
  /*take xs, and return sorted version of List[Int]   */
  def isort(xs: List[Int]): List[Int] =
  if (xs.isEmpty) Nil
  else insert(xs.head, isort(xs.tail))

  def isortPatternM(xs: List[Int]): List[Int] = xs match {
    case List() => List() //If input is empty List return empty List
/* if input consist of y(head) ys(tail List) */
    case y :: ys => insert(y, isortPatternM((ys)))
  }
  /* List(7,3,9,2) -> List(2, 3, 7, 9)
  sort([7,3,9,2] =>
  * xs=[7,3,9,2], y =7 , ys =[3,9,2] => insert(7, isort([3,9,2])) =>
  * insert(7, (xs=[3,9,2], y =3 , ys=[9,2])) => insert(7, insert(3, isort[9,2])
  * => insert(7, insert(3, (xs=[9,2], y = 9, ys =[2]))) => insert (7, insert(3, insert(9, isort[2])))
  * => xs=[2], y = 2, ys =Nil
  * =>insert (7, insert(3, insert(9, insert(2, isort(Nil))))) => insert (7, insert(3, insert(9, insert(2, Nil))))
  * => insert (7, insert(3, insert(9, 2)))
  * */
}

