val xs = Array(1,2,3,44)
xs map (x => x * 2)
val s= "Ranges is Single Object with fields Such As lower, bound, upperbound, step value"
s filter (s => s.isUpper)
val flatMapNumber = Array(1,2,3,44) flatMap (c => Array('.', c ))
println(" Array(1,2,3,44) flatMap (c => Array(0, c )) : " + flatMapNumber)
xs.sum
1 to 5
val n = 7
val vecRes = (1 until n) map (i => (1 until i) map (j => (i, j)))

for (e <- vecRes)
  println(e)


  //class Ploy(val terms: Map[Int, Double]) extends Poly { //Map[key(exponential), value(coefficient)]

class Poly3(val terms : Map[Int, Double]) { //Map[key(exponential), value(coefficient)]
  def + (other: Poly3) = new Poly3(terms ++ (other.terms map adjust))
  def adjust(term_elem: (Int, Double)): (Int, Double) = {
    val (exp, coeff) = term_elem
    terms get exp match {
      case Some(coeff1) => exp -> (coeff + coeff1)
      case None => exp -> coeff
    }
  }
  override def toString =
      (for ((exp, coeff) <- terms.toList.sorted.reverse) yield  coeff + "x ^" + exp) mkString " + "
  }//end of class Poly3
val p1 = new Poly3(Map( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p2 = new Poly3(Map(0 -> 3.0, 3 -> 7.0))
println("poly add result : "+ (p1 + p2) )

class Poly4(terms0 : Map[Int, Double]) { //Map[key(exponential), value(coefficient)]
  val terms = terms0 withDefaultValue 0.0
  def + (other: Poly4) = new Poly4(terms ++ (other.terms map adjust))
  def adjust(term_elem: (Int, Double)): (Int, Double) = {
   val (exp, coeff) = term_elem
    exp -> (coeff + terms(exp))
    }
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield  coeff + "x ^" + exp) mkString " + "
  }//end of class Poly4
val p41 = new Poly3(Map( 1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
val p42 = new Poly3(Map(0 -> 3.0, 3 -> 7.0))
println("poly add result : "+ (p41 + p42) )

//test
val map1 = Map(3 -> 3, 1 -> 2, 0 -> 5 )
val map2 = Map(5 -> 2, 3 -> 5 )
//duplicated key , just replace value of second operand(map2)'s value with key
println( map1 ++ map2)

val xs_t = Vector(1,2,3)
val ys_t = Vector(1,2)
val re1 = (xs_t zip ys_t) map (elem => elem._1 * elem._2) sum

println("product of Vector(1,2,3) and Vector(1,2) : " + re1)
println(" Vector(1,2,3) zip Vector(1, 2) : " + (Vector(1,2,3) zip Vector(1,2)))
//Vector((1,1), (2,2))