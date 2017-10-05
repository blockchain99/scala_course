package scala

object list_example2 {
  def main(args: Array[String]): Unit = {

  }
}
//abstract class List2[+A]{
//  def isEmpty: Boolean = this match {
//    case Nil => true
//    case x :: xs => false
//  }
//  def head: A = this match {
//    case Nil => sys.error("Nil.head")
//    case x :: xs => x
//  }
//  def tail: List[A] = this match {
//    case Nil => sys.error("Nil.tail")
//    case x :: xs => xs
//  }
//  def length: Int = this match {
//    case Nil => 0
//    case x :: xs => 1 + xs.length
//  }
//}