import org.apache.spark.sql.SparkSession

object word2 {
//    def main(args: Array[String]) {
//      // create Spark context with Spark configuration
//      val sc = new SparkContext(new SparkConf().setAppName("Spark Count"))
//
//      // get threshold
//      val threshold = args(1).toInt
//
//      // read in text file and split each document into words
//      val tokenized = sc.textFile(args(0)).flatMap(_.split(" "))
//
//      // count the occurrence of each word
//      val wordCounts = tokenized.map((_, 1)).reduceByKey(_ + _)
//
//      // filter out words with fewer than threshold occurrences
//      val filtered = wordCounts.filter(_._2 >= threshold)
//
//      // count characters
//      val charCounts = filtered.flatMap(_._1.toCharArray).map((_, 1)).reduceByKey(_ + _)
//
//      System.out.println(charCounts.collect().mkString(", "))
//    }
  def main(args: Array[String]): Unit = {
  val sparkSession = SparkSession.builder
      .master("local")
    .appName("Spark word count")
    .getOrCreate()

  val lines = Seq("This is the first line",
  "This is the second line",
  "This is the third line")

  val rdd = sparkSession.sparkContext.parallelize(lines)

  val result = rdd
    .flatMap(line => line.split(" "))  //split each line by " ", which make all blank separated lines in a line./* lazy*/
    .map(word => (word, 1))  //for each word, assign number 1 /* transformation : lazy  */
//    .reduceByKey((accumulatedValue, currentValue) => accumulatedValue + currentValue)  //same as below
      .reduceByKey(_ + _)   /* eager */
    .collect().toList
  println(result)

  val totalChar = rdd
    .map(_.length)
    .reduce(_+_)
  println("*********totalChar : "+ totalChar)
  /*val wordsRdd = sparkSession.sparkContext.parellelize(lines)
    val lengthsRdd = wordsRdd.map(_.length)
    //wordsRdd.map(x => x.length)
    val totalChars = lengthsRdd.reduce(_ + _)
     // lengthsRdd.reduce((acc, curr) => acc + curr  */

/*Creating a pair RDD using the first word as the key in Scala */
  val result2 = rdd
    .map(x => (x.split(" ")(0), x))
  println("**** result2 :"+result2)
}


//  def main(args: Array[String]) =
//  {
//    val conf = new SparkConf()
//      .setMaster("local[*]")
//      .setAppName("Test Spark")
//      .set("spark.executor.momory", "2g")
//    val sc = new SparkContext(conf)
//    val lines = sc.parallelize(Seq("This is first line", "This is second lines", "This is third line"))
//    val counts = lines.flatMap(line => line.split(" "))
//      .map(word => (word, 1))
//      .reduceByKey(_ + _)
//    counts.foreach(println)
//  }
}