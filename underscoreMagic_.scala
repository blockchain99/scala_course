object underscoreMagic_ {
  /*Scala is a functional language. So we can treat function as a normal variable.
   If you try to assign a function to a new variable, the function will be invoked
   and the result will be assigned to the variable. This confusion occurs due to
   the optional braces for method invocation. We should use _ after the function name
   to assign it to another variable.
*/
  class TestFunc(x: Double) {
    def assignFunctionToVar(x:Double) = {
       println("result is "+ (x*x+100))
    }
    var assignFunctionToVarLike = assignFunctionToVar _  //_ : assign the function to var w/o executing this function.
  }//end of class Test

  /* setter: The getter name is same as the variable name and _= is added for setter name.  */
  class TestProperties {
    private var a = 0
    def age = a //getter
    def age_=(n:Int) = {  //setter
      require(n>0)
      a = n
    }
  }

  /* Pattern matching with embedded _*/
 /* class LocationDao {
    val db = DbProvider.db

    // Database tables
    val devices = Devices.devices
    val locations = Locations.locations
    val programs = Programs.programs
    val accessTokens = AccessTokens.accessTokens

    def loginDevice(deviceSerialNumber: String, login: String, password: String): Either[Error, LocationResponse] = {
      Try("123".toInt) match {
        case Success(val1) => {
          val val2 = Some(val1).map(_ * 2)
          val2 match {
            case Some(val2value) => {
              val val3 = Some(val2value - 55)
              val3 match {
                case Some(val4) => Some(val4)
                case None => None
              }
            }
            case None => None
          }
          case f: Failure => None
        }
      }
    } //end of def loinDevice
  }//end of class LocationDao
  */

  def main(args: Array[String]): Unit = {
   val testFunc = new TestFunc(2.2)
   println(testFunc.assignFunctionToVarLike)  //It calls function,assignFunctionToVar in class Test but w/o passing argument
    println(testFunc.assignFunctionToVar(5.1))

    /*test properties */
    val testProperties = new TestProperties
    testProperties.age_=(42)  //setter using _
    println("TestProperties result : "+testProperties.age) //getter_

    /* anonymous function :
*  The _ acts as a placeholder for parameters in the anonymous function.
*  The _ should be used only once, But we can use two or more underscores
*  to refer different parameters*/
    println(List(1,2,3,4,5).foldLeft(0)(_+_))
    println(List(1,2,3,4,5).foldLeft(0)((x,y) => x+y))

    println(List(1,2,3,4,5).reduceLeft(_+_))
    println(List(1,2,3,4,5).reduceLeft((x,y) => x+y))

    println("(1): ")
    List(1,2,3,4,5).foreach(print(_));println()
    println("(2): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(x => print(x + " , "));println()
    println("(3): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(print(_));println()
    println("(4): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(print);println()

    def TestPattenMatch[Int](x:Int):String = x  match {
      case 1 => "one"
      case 2 => "two"
      case _ => "more than two"
    }

    def TestType[T](x:T):String = x match {  //output type :String can be omitted
      case _: String => "String"
      case _: Double => "Double"
      case _  => "None"
    }

    val testProperties2 = TestPattenMatch(3)
    println("TestPattenMatch(3) :" +testProperties2)


  }//end of main

}
