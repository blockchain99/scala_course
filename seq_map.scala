object seq_map {
  def main(args: Array[String]): Unit = {
    val test1 = List(1,2,3,4)
    println("map result : "+ (test1 map (x =>  x* 2)) )
    println(test1.map(x=>x*2))

  }//end of main

}//end of object seq_map
/*Idealize map take function from List element type T to some arbitary type U,
  so, U is type parameter of map and give back a List of U
   What It does is pattern matching of List,itself *  */
//abstract class List[+T] {
//  def map[U](f: T => U): List[U] = this match {
//      //just prepand
//   case x :: xs => f(x) :: xs.map(f)   //apply f to the head & map f of the tail, then compose the result with con(::)
//   case Nil => Nil
//  }
//}
//
//abstract class List[+T] {
//  def flatMap[U](f: T => List[U]): List[U] = this match {
//      //concatenate
//    case x :: xs => f(x) ++ xs.flatMap(f)   //apply f to the head & map f of the tail, then compose the result with con(::)
//    case Nil => Nil
//  }
//}
//abstract class List[+T] {
//  def filter(p: T => Boolean): List[T] = this match {
//    case x :: xs =>
//      if (p(x)) x::xs.filter(p) else xs.filter(p)
//    case Nil => Nil
//  }
//}
