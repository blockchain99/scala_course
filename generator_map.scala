/*generator with map and flatMap */
object generator_map {
  def main(args: Array[String]): Unit = {

    val integers = new Generator[Int] {
      val rand = new java.util.Random
      def generate = rand.nextInt()
    }

    trait Generator[+T]{
      self =>  //an alias for "this"
      def generate: T

      def map[S](f: T => S): Generator[S] = new Generator[S] {
        def generate = f(self.generate)
        /*if we create just "generate", which is "this.generate" in def map
        cause infinite loop in def map, whereas self.generate, one further out,
         not unlimited loop
          it can achieve "Generator.this.generate"*/
      }
      //  def map[S](f: T => S): Generator[S] = new Generator[S] {
      //    def generate = f(Generator.this.generate)
      //  }
      def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
        def generate = f(self.generate).generate
      }
    }//end of trait Generator
    /* booleans Generator, What does this definition resolve to ? */
    val booleans = for(x <- integers) yield x > 0 //1.
//    val booleans = integers map {x => x > 0}    //2.
//    val booleans = new Generator[Boolean] {     //3.  //error
//      def generate =(x: Int => x > 0)(integers.generate)
//    }
//    val booleans = new Generator[Boolean] {     //4.
//      def generate = integers.generate > 0
//    }
//    def pairs[T, U](t: Generator[T], u: Generator[U]) = t flatMap {
//      x => u map { y => (x, y) } }
    def pairs[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
      def generate = (t.generate, u.generate)
    }
    def single[T](x: T): Generator[T] = new Generator[T] {
      def generate = x
    }
    def choose(lo: Int, hi: Int): Generator[Int] =
      for (x <- integers) yield lo + x % (hi - lo)
    def oneOf[T](xs: T*): Generator[T] =
      for (idx <- choose(0, xs.length)) yield xs(idx)

    /* list is either an empty list or non-empty list  */
    def lists: Generator[List[Int]] = for {
      isEmpty <- booleans
      list <- if (isEmpty) emptyLists else nonEmptyLists
    } yield list
    def emptyLists = single(Nil)
    def nonEmptyLists = for {
      head <- integers
      tail <- lists
    } yield head :: tail
  }//end of main
}
