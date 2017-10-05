/* class Map[key, value] extends the collection type Iterable[(key, value)]
 * Therefore, Maps support the same collection operations as other iterables do.
  * Note : maps extend iterables of key/value pairs */
object map_ex {
  def main(args: Array[String]): Unit = {
    val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10 )
    val romanNumerals2 = Map("I" -> 1, "V" -> 5, "X" -> 10 ):Map[String, Int]
    val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
    val capitalOfCountry2 = Map("US" -> "Washington", "Switzerland" -> "Bern"):Map[String, String]
    println("Map(\"I\" -> 1, \"V\" -> 5, \"X\" -> 10 ) : " + romanNumerals)
    println("Map(\"I\" -> 1, \"V\" -> 5, \"X\" -> 10 ) : " + romanNumerals2)
    val countryOfCapital = capitalOfCountry map {
      case(x, y) => (y, x)
    }
    /* key value swithch */
    println(" capitalOfCountry map { case(x, y) => (y, x) }: " + countryOfCapital)
//    println(capitalOfCountry("Andorra")) //key not found error
    /* before just check existence */
    println(capitalOfCountry get "US")
    println(capitalOfCountry get "Amdorra")
    /* key -> value : pari (key, value) */
//    trait Option[+A]
//    case class Some[+A](value: A) extends Option[A]
//    object None extends Option[Nothing]

    def showCapital(country: String) = capitalOfCountry get country match {
      case Some(capital) => capital
      case None => "missing data"
    }
    println(showCapital("US"))
    println(showCapital("Andorra"))
    val fruit = List("apple", "pear", "orange", "pineapple")
    val sortedLength = fruit.sortWith(_.length < _.length)
    println("fruit.sortWith(_.length < _.length) : "+ sortedLength)
    val sorted = fruit.sorted
    println("fruit.sorted : " + sorted)
    val groupBy = fruit groupBy (_.head)
    println("fruit groupBy (_.head) : " + groupBy)
    /*map is partial function, where lead to an exception when no key in the map
        * But withDefaultValue turns a mp into a total function: */
    val cap1 = capitalOfCountry withDefaultValue "<unknown>"
    println("capitalOfCountry withDefaultValue \"<unknown>\"- cap1(\"Andorra\") : " + cap1("Andorra"))
    val p1 = new Poly(Map( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Poly(Map(0 -> 3.0, 3 -> 7.0))
    println("polynominal add result 1 : " + p1 + p2)
//    println("* p1.terms(7) : " + p1.terms(7))  //error since corresponding value( for key 7) is missing

    val p21 = new Poly2(Map( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p22 = new Poly2(Map(0 -> 3.0, 3 -> 7.0))
    println("polynominal add result with withDefaultValue 0.0 : "+p21 + p22)
    println("* p21.terms(7) : " + p21.terms(7))

    val pWoMap1 = new Poly5( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //Now error since w/o Map, so .toMap needed
    val pWoMap2 = new Poly5(Map(0 -> 3.0, 3 -> 7.0))
/* foldLeft */
    val pfoldLeft1 = new PolyFoldLeft( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2) //Now error since w/o Map, so .toMap needed
    val pfoldLeft2 = new PolyFoldLeft(Map(0 -> 3.0, 3 -> 7.0))

  }//end of main
  class Poly(val terms: Map[Int, Double]) { //Map[key(exponential), value(coefficient)]
    //when exp of this.terms == exp of other.terms, just return exp of other superimposed
    // instead of adding tow coefficients
    def +(other: Poly) = new Poly(terms ++ (other.terms map adjust))

    def adjust(term_elem: (Int, Double)): (Int, Double) = { //change to pair(Int, Double), term_elem
      val (exp, coeff) = term_elem //make other.terms.(exp, coeff) element pair from other.terms's term_elem sequence
      /* If a value corresponding to a given key,"exp" has been found, Then get produces Some(value,"coeff1") ,
      Or if the given key, "exp" is not defined in the Map, it produce None . */
      terms get exp match { //if (this.)terms,Map have "exp" key element & match following case
        //if there is this.terrms.coeff1,make Map with key(exp),value(coeff+coeff1)
        case Some(coeff1) => exp -> (coeff + coeff1)
        case None => exp -> coeff
      }//end of match
    }//end of def adjust
    override def toString =
        (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }//end of class Poly

  /* withDefaultValue(0.0) */
  class Poly2(terms0 : Map[Int, Double]) { //instead of having field, "terms"
//    val terms = terms0 withDefaultValue(0.0)       //create new parameter with withDefaultValue(0.0)
    val terms = terms0 withDefaultValue 0.0       //create new parameter with withDefaultValue(0.0)
  def +(other: Poly2) = new Poly2(terms ++ (other.terms map adjust))

    def adjust(term_elem: (Int, Double)): (Int, Double) = { //change to pair(Int, Double), term_elem
      val (exp, coeff) = term_elem
      exp -> (coeff + terms(exp))
    }//end of def adjust
   override def toString =
        (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }//end of class Poly2

  class Poly5(terms0 : Map[Int, Double]) { //instead of having field, "terms"
    /*auxiliary constructor with undefined numbers of input type (int, Double)pair
    * (Int, Double)type is changed to Map by .toMap */
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    //    val terms = terms0 withDefaultValue(0.0)       //create new parameter with withDefaultValue(0.0)
    val terms = terms0 withDefaultValue 0.0       //create new parameter with withDefaultValue(0.0)
    def +(other: Poly5) = new Poly5(terms ++ (other.terms map adjust))

    def adjust(term_elem: (Int, Double)): (Int, Double) = { //change to pair(Int, Double), term_elem
      val (exp, coeff) = term_elem
      exp -> (coeff + terms(exp))  //make Map with key, exp and value, (coeff + terms(exp) i.e current key exp's value
    }//end of def adjust
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }//end of class Poly5

/*  instad of ++ using (xs foldLeft ini), it is more efficient, since ++ version create aditional list
 * to concatenae (terms ++ (other.terms map addTerm) ) */
  class PolyFoldLeft(terms0 : Map[Int, Double]) { //instead of having field, "terms"
    /*auxiliary constructor with undefined numbers of input type (int, Double)pair
    * (Int, Double)type is changed to Map by .toMap */
    def this(bindings: (Int, Double)*) = this(bindings.toMap)  //( 3, 2) changed to 3 -> 2
    //    val terms = terms0 withDefaultValue(0.0)       //create new parameter with withDefaultValue(0.0)
    val terms = terms0 withDefaultValue 0.0       //create new parameter with withDefaultValue(0.0)
  /*  if other.terms is 0, return current, terms returned, so among other.terms, 0 element returned  */
    def +(other: PolyFoldLeft) = new PolyFoldLeft((other.terms foldLeft terms)(addTerm))
    /*input is 0 element, so terms  */
    def addTerm(terms: Map[Int,Double], term_elem: (Int, Double)): Map[Int, Double] = { //change to pair(Int, Double), term_elem
      val (exp, coeff) = term_elem
      /*Instead of returning single binding,  add this binding to the given terms map */
      terms + (exp -> (coeff + terms(exp)))  //make Map with key, exp and value, (coeff + terms(exp) i.e current key exp's value
    }//end of def adjust
    override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
  }//end of class Poly5
}//end of object map_ex
/*  error */




