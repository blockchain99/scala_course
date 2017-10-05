import org.apache.spark.{SparkConf, SparkContext}

object word3 {
  def main(args: Array[String]): Unit = {
    // Create a Scala Spark Context.
    val conf = new SparkConf().setMaster("local").setAppName("wordCount")
    val sc = new SparkContext(conf)
    // Load our input data.
    val input = sc.textFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\sample.txt")
    // Split it up into words.
    val words = input.flatMap(line => line.split(" "))
    // Transform into pairs and count.
    val counts = words.map(word => (word, 1)).reduceByKey{case (x, y) => x + y}
    // Save the word count back out to a text file, causing evaluation.
    counts.saveAsTextFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\sample_out.txt")

    /****** reduceByKey explained **********************
      * val lines = sc.textFile("data.txt")
      * val pairs = lines.map(s => (s, 1))
      * val counts = pairs.reduceByKey((a, b) => a + b)
      **************************************************
    pairs.reduce((accumulatedValue: List[(String, Int)], currentValue: (String, Int)) => {
  //Turn the accumulated value into a true key->value mapping
  val accumAsMap = accumulatedValue.toMap
  //Try to get the key's current value if we've already encountered it
  accumAsMap.get(currentValue._1) match {  //get(key) produce key's value
    //If we have encountered it, then add the new value to the existing value and overwrite the old
    case Some(value : Int) => (accumAsMap + (currentValue._1 -> (value + currentValue._2))).toList
    //If we have NOT encountered it, then simply add it to the list
    case None => currentValue :: accumulatedValue
  }
})  */

  }//end of main
}//end of object word3
