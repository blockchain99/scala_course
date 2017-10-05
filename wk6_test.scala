object wk6_test {


  def main(args: Array[String]): Unit = {
    val xs = Array(1,2,3,44)
    val mapArray = xs map (x => x * 2)
    println("xs map (x => x * 2) : " + mapArray)
    for (e <- mapArray)
      println(e)

    val s= "Ranges is Single Object with fields "
    val filterString = s filter (c => c.isUpper)
    println("s filter (c => c.isUpper) : " + filterString)
    val existsString = s exists (c => c.isUpper)
    println("s exists (c => c.isUpper) : " + existsString)
    val forallString = s forall (c => c.isUpper)
    println("s forall (c => c.isUpper) : " + forallString)
    val pairs = List(1,2,3) zip s
    println(" List(1,2,3) zip s : " + pairs)
    val flatMapString = s flatMap(c => List('.', c))
    /*xs flatMap f -> applies collection valued function f to all elements of xs and
    concatenated the results  */
    println("s flatMap(c => List(\".\", c)) : "+ flatMapString)
    val flatMapNumber = Array(1,2,3,44) flatMap (c => Array(0, c ))
    println(" Array(1,2,3,44) flatMap (c => Array(0, c )) : " + flatMapNumber)
    for (e <- flatMapNumber)
      println(e)
    val oneToM = 1 to 5  //type is Range
    val oneToN = 1 to 3   //type is Range
    val oneToMV = Vector(1.0,2,3,4)
    val oneToNV = Vector(1.0,2,3)
    println("combination_ListX_ListY : " + combination_RangeX_RangeY(oneToM, oneToN))
    println("scalaProduct : "+ scalaProduct(oneToMV, oneToNV))
    println("* scalaProductFor : "+ scalaProductFor(oneToMV, oneToNV))
    println("def isPrime(n: Int): Boolean = (2 to n) forall (d => n % d != 0) : " + isPrime(7))
    val givenNum = 7
    val forVector = pairOfIntFor(givenNum)
    val resVector = pairOfInt(givenNum)
    println("$$ for syntax -pair of int  : 1 <= j < i < n :  " +  forVector)
//    for (e <- forVector)
//      println(e)
    println(" pair of int(i + j shd be prime) : 1 <= j < i < n :  " +  resVector)
    println("flattenSeq : "+ flattenSeq(7))
    println("flattenMap Seq : "+ flatMapSeq(7))

  }//end of main
/* map flatMap test */
  val l = List(1,2,3,4)
  val l_map = l.map(x => x * 2)
  println("l.map(x => x * 2) : " + l_map)
  def f(x: Int) = if (x > 2) Some(x) else None
  val l_map2 = l.map(f)
  val l_map2_1 = l.map(x=>f(x)) //same as above
  println("l.map(f) : "+ l_map2)
  /*flatMap works applying a function that returns a sequence for each element in the list,
  and flattening the results into the original list.   */
  def g(x: Int) = List(x-1, x, x+1)
  println("l.map(g) : " + l.map(g))
//  println("l.map(x => g(x)) : " + l.map(x => g(x)))  //same as above
//  println(l.map(x => List(x-1, x, x+1)))  //same as above
  println("* l.flatMap(g) : "+ l.flatMap(g))

////  def combination_ListX_ListY[T](xs: List[T], ys: List[T]) =
//  def combination_RangeX_RangeY(xs: Range, ys: Range) =
//    (xs) flatMap (x => (ys) map (y => (x, y)))  //xs(range) becomes x(a element in xs range) , ys becomes y

  def combination_RangeX_RangeY(xs: Range, ys: Range) =
    xs flatMap (x => ys map (y => (x, y)))  //xs(range) becomes x(a element in xs range) , ys becomes y

  def scalaProduct(xs: Vector[Double], ys: Vector[Double]): Double =
//    (xs zip ys).map( xy => xy._1 * xy._2).sum  //same as below
    (xs zip ys) map( elem => elem._1 * elem._2) sum
  def scalaProductFor(xs: Vector[Double], ys: Vector[Double]): Double =
  //    (xs zip ys).map( xy => xy._1 * xy._2).sum  //same as below
//    (for ((x,y) <- xs zip ys) yield x * y).sum
    (for ( elem <- (xs zip ys))  yield (  elem._1 * elem._2)).sum
  /* pattern matching function
   * Generally, the function value  { case p1 => e1 ... case pn => en}
    * is equivalent to
    * x => x match { case p1 => e1 ... case pn => en }*/
  def scalarProduct2(xs: Vector[Double], ys: Vector[Double]): Double =
//    (xs zip ys).map{ case (x, y) => x * y }.sum
    (xs zip ys) map { case (x, y) => x * y } sum
  /* A number n is prime if the only divisors of n are 1 and itself
  * so, from 2 to itself,n. it can't be divided by each of these numbers-> n is prime*/
//  def isPrime(n: Int): Boolean = (2 to n) forall (d => n % d != 0)// wrong since n should not be included
  def isPrime(n: Int): Boolean = (2 until n) forall (d => n % d != 0)// until n : n is not included.
  /*for (s) yield e
    * generator : p <- e form: p is a pattern, e is expression
     * filter : if f (f is boolean expression) */
  def pairOfIntFor(n: Int): Unit = {  //only return created object name
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield  (i, j)  //TODO: how to print each element instad of instance name ?
  }

  /* not resolve i, filter,  error !*/
//  def isPrimeFlatMap(n: Int) =
//    (1 until n) flatMap (i => (1 until i)) map (j => (i, j))) filter (pair =>
//     isPrime(pair._1 + pair._2))

  /* Generate the sequence of all pairs of integers (i,j) such that
     * 1 <= j < i < n
     * Filer the pair i + j is prime :  " < n" can be  "until n" */
  /*but output is Vector, But we need to be list pair   */
  val xss = (1 until 7) map (i => (1 until i) map (j => (i, j)))
  def pairOfInt(n: Int) {
//    (1 until n) map (i => (1 until i) map (j => (i, j)))   //only object name return
    (1 until n) map (i => (1 until i) map (j => (i, j))) foreach(println)
  }
  /*sequence of sequences is xss, combine all the sub-sequences using flodRight with ++   */
//  def foldRightSeq(n: Int) = {
//    val xss2 = (1 until n) map (i => (1 until i) map (j => (i, j)))
//    (xss2 foldRight Seq[Int]())(_ ++ _)  //type mismatch
//  }
  def flattenSeq(n: Int) = {
  ((1 until n) map (i =>
    (1 until i) map (j => (i, j))) ).flatten
//  ((1 until n) map (i =>
//    (1 until i) map (j => (i, j))) ) flatten
  /*xs flatMap f = (xs map f).flatten
    (1 until n) flatMap ( i => (1 until i) map (k => (i, j)))
       * */
  }
//  def flatMapSeq(n: Int): Any =  (1 until n) flatMap (i =>
  def flatMapSeq(n: Int) =  (1 until n) flatMap (i =>
    (1 until i) map (j => (i, j)))

}
