object jsonPatterMatching {
  def main(args: Array[String]) = {
    /*method that returns the string representation JSON data */
//    def show(json: JSON): String = json match {
//      /* show recursively all the elements concatenating using mkString  */
//      case JSeq(elems) =>
//        "[" + (elems map show mkString "," )+ "]"
//      case JObj(bindings) =>
//        val assocs = bindings map {
////        val assocs = bindings match {
//          case (key, value) => "\"" + key + "\": " + show(value)
//        }
//        "{" + (assocs mkString ", ") + "}" //{(k,v),(k,v),...}
//      case JNum(num) => num.toString
//      case JStr(str) => '\"' + str + '\"'
//      case JBool(b)  => b.toString
//      case JNull     => "null"
//        }
//    type JBinding = (String, JSON)
    /* In Scala, every concrete type is the type of some class or trait.
    * The function type is no exception A type like JBinding => String is just
    * a shorthand  for  scala.Function1[JBinding, String] i.e, scala.Function1 is a trait
     * JBinding and String are its type arguments. */

  /* pattern matching block, expands to the Function1 instance */
//  new Function1[JBinding, String] {
//    def apply(x: JBinding) = x match {
//      case (key, value) => key + "; " +show(value)
//    }
//  }
    println("*********** Partial Matches ************")
    /* check whether given argument is defined in function   */
    val f: String => String = { case "ping" => "pong"}
    f("ping")
//    f("abc") //when excute erro happen, so before excute, Can we know?
    val fPartial: PartialFunction[String, String] = { case "ping" => "pong"}
    fPartial.isDefinedAt("ping")  //true
    fPartial.isDefinedAt("pong")   //false
/* Partial Function Objects
  * If the expected type is a PartialFuntion, the Scala compiler will expand */
    // {case "ping" => "pong" }
   /*  as follows  * */
    new PartialFunction[String, String] {
      def apply(x: String) = x match {
        case "ping" => "pong"
      }
      def isDefinedAt(x: String) = x match {
        case "ping" => true
        case _ => false
      }
    }

  }//end of main
} //end of object jsonPatternMatching

//abstract class JSON
//case class JSeq (elem: List[JSON]) extends JSON
//case class JObj (bindings: Map[String, JSON]) extends JSON
//case class JNum (num: Double) extends JSON
//case class JStr (str: String) extends JSON
//case class JBool (b: Boolean) extends JSON
//case object JNull extends JSON
//
//trait Function1[-A, +R] {
//  def apply(x: A): R
//}
/* pattern matching block   */
//( case (key, value) => key + ": " + value) }
/* "************* Subclassing Function *************" */
/* nice aspect of functions being traits - we can subclass the function type */
/*maps are functions from keys to values*/
trait Map[Key, Value] extends (Key => Value)
/*Sequences are functions from Int inidices to values
* That's why we can write elems(i) for sequence (and array) indexing.
* in Java elems[i]-so, this is not function.*/
trait Seq[Elem] extends (Int => Elem)

trait partialFunction[-A, +R] extends Function1[A, R] {
  def apply(x: A): R
  def isDefinedAt(x: A): R
}


