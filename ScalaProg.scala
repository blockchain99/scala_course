import java.security.KeyStore.TrustedCertificateEntry

object ScalaProg {
  def main(args: Array[String]): Unit = {
    println("Scala first program")
    val a = 10 + 3 * 5 / 2  //immutable
    println("a is :" + a)
    var myName = "Ys park"  //mutable
    println("b is :" + myName)
    println( myName.size)  //String's size : string_name.size
    println(myName.startsWith("Y"))
//    val myAge_val = 40  //val : immutable
//    println(myAge_val)
//    myAge_val = 50  //so error
//    println(myAge_val)
    var myAge_var = 40  //var : mutable
    println(myAge_var)
    myAge_var = 50
    println(myAge_var)
    //Byte
    //Boolean
    //Char
    //Short
    //Int
    //Long
    //Float
    //Double
    /* 
     * multi line comments.
     * 
     */
    val num13 = 1.999999999
    println(num13)
    val lgprime = BigInt("654894575733333333333331111111111111444444444444444288888888885525")
    val lgprime2 : Any = BigInt("6548945757333333333333311111111111114444444444444442888844477777")
    def flgprime2[T](v: T) = v match {
      case _: Int => "Int"
      case _: BigInt => "BigInt"
      case _         => "I don't know"  
    }
    println("resut:",flgprime2(lgprime2))
    println("*******++++++++++++++**************")
    val pi50 = BigDecimal("3.1415925612872549475764647475753")
    println(pi50 )
    println(pi50.scale)
    println(0.00000000000000000000000000001 + pi50)
    println((0.00000000000000000000000000001 + pi50).scale)

    println("**********************************")
    val x1: Any = 5                 //Int
    val x2: Any = "This is String"  //String
    val x3: Any = 1.99999           //Float
    //value : v, Type : T - function of Type T produce "Int", "String",.. with input val: v
    def f[T](v: T) = v match {  //function is for type T - f[T]
      case _: Int    => "Int"
      case _: String => "String"
      case _         => "Unknown"
      }
    println("f(Int_input) : "+f(x1))
    println("f(String_input) : "+f(x2))
    println("f(esle_input)) : "+f(x3))

   
    println("------------------------------")
    val x = 'c'
    val y = 5
    val z: Any = 5
    // ClassTag will also let you use type parameters you received on match.
    import scala.reflect.ClassTag
    def l[A, B: ClassTag](a: A, b: B) = a match { //function is for type A,B , which is ClassTag- l[A, B :ClassTag];
      case _: B => "A is a B"
      case _ => "A is not a B"
    }
    println(l(x, y)) // A (Char) is not a B (Int)
    println(l(x, z)) // A (Char) is a B (Any)


    val list = "apple" :: "banana" :: 1 :: 2 :: Nil  //List with 4 elements
    println("** list with Any type element : "+list)
    var strings = list.filter{
      case _ : String => true
      case _ => false
    }
    println("** list with string : "+ strings)

    /* function */
    def onlyStrings(a: Any) = a match {
      case _: String => true
//      case s: String => true //same as above
      case _ => false
    }
    val strings_func = list.filter(onlyStrings)
    println("* strings_func : "+ strings_func)


    val twoElementList = "persimon" :: 55 :: Nil
    println(twoElementList)

    def id[T](x: T) = x
    val x22 = id(322)
    println(x22, x22.getClass)  //(322,int)
    val x33 = id(322.00)
    println(x33,x33.getClass)   //(322.0,double)
//    val result = list[T].filter(T => String)

    /* abstract type */
    trait C[A] {
      def get : A
      def doit(a:A):A
    }

    var c0: C[Int]= new C[Int] {
      def get = 3
      def doit(x: Int) = x*100
    }
    var result0 = c0.get
    println("** abstract type(c0.get) : "+result0)
    var result0_doit = c0.doit(result0)
    println("** abstract type(c0.doit) : "+result0_doit)

    /* parameter  */
    trait C2 {
      type A
      def get : A
      def doit(a:A):A
    }
    //compiles
    def p(c:C[Int]) = c.doit(c.get)

    var c : C2 = new C2 {
      type A = Int
      def get = 3
      def doit(x: Int) = x*100
    }

    var result = c.get
    println("* c.get : "+result)


    /* type mismatch :   */
//    var result2 = c.doit(c.get)
//    println("c.doit(c.get) : "+result2)


    // doesn't compile
    // Because it returns A. From the signature of p2
    // it is impossible to know what p2 returns.
    // There are several ways to fix this problem.
    // One make the method return Unit:

    def p2(c:C2) = c.doit(c.get)
    // compiles because the internals of C2 does not leak out
    def p3(c:C2):Unit = c.doit(c.get)

    /*Another fix would be to change doit to return Unit or an explicit return value like Int */

    trait C3 {
      type A
      def get : A
      def doit(a:A):Int
    }
    // compiles correctly
    def p4(c:C3) = c.doit(c.get)

  }
}