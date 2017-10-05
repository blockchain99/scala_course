import java.util.Random


object generator {
  def main(args: Array[String]): Unit = {
    val rand1 = new Random
    println("* random number : "+ rand1)
    println("* rand1.nextInt() " + rand1.nextInt())
//    val integers = new Generator[Int] {
//      val rand = new java.util.Random
//      def generate = rand.nextInt()
//    }
    val integers = new Generator[Int] {
      def generate = scala.util.Random.nextInt()
    }
    val booleans = new Generator[Boolean] {
      def generate = integers.generate > 0
    }

    val pairs = new Generator[(Int, Int)] {
      def generate = (integers.generate, integers.generate)
    }

//    val booleans = for (x <- integers) yield x > 0 //error: not resolve >

//    def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
//      x <- t
//      y <- u
//    } yield (x, y)

//    val booleans = integers map (x => x > 0)
    val booleans2 = List(1,2,3,4) map (x => x > 0)

    def test[T](g: Generator[T], numTimes: Int = 100)
               (test: T => Boolean): Unit = {
      for (i <- 0 until numTimes) {
        val value = g.generate
        assert(test(value), "test failed for " +value)
      }
      println("passed " +numTimes+" tests")
    }
  }//end of main
//  def pairsM[T, U](t: Generator[T], u: Generator[U]) =
//    t flatMap (x => u map (y => (x, y)))
//
}
trait Generator[+T] {
  def generate: T
}
