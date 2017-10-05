import org.apache.spark.{SparkConf, SparkContext}

object rddTest2 {
  def main(args: Array[String]): Unit = {
    val conf2 = new SparkConf().setMaster("local").setAppName("rddTest2")
    val sc2 = new SparkContext(conf2)
    case class Person(name: String, age: Int)
    val people = Array(Person("bob", 30), Person("ann", 32), Person("carl", 19), Person("Park", 39))
    val rdd = sc2.parallelize(people, 3)
    val orderedRdd = rdd.takeOrdered(3)(Ordering[Int].reverse.on(x => x.age))
    println("*** rdd.takeOrdered(1) : "+orderedRdd)

    orderedRdd.foreach(println)
  }
}
