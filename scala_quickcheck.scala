
//import org.scalacheck.Prop.forAll
import org.scalacheck.Prop.forAll
import org.scalacheck.Properties


object scala_quickcheck extends Properties("String") {
/* second property was not quite right. (arguments that make the property fail,
 * two empty strings. ) */
  val propConcatLists = forAll { (l1: List[Int], l2: List[Int]) =>
    l1.size + l2.size == (l1 ::: l2).size }

    property("startsWith") = forAll { (a: String, b: String) =>
      (a+b).startsWith(a)
    }

    property("concatenate") = forAll { (a: String, b: String) =>
      (a+b).length > a.length && (a+b).length > b.length
    }

    property("substring") = forAll { (a: String, b: String, c: String) =>
      (a+b+c).substring(a.length, a.length+b.length) == b
    }
}
