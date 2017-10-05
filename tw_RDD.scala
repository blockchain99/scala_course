import org.apache.spark.sql.SparkSession

object tw_RDD {
  def main(args: Array[String]): Unit = {
    val sparkSession_tw = SparkSession.builder
      .master("local")
      .appName("Twinkle line")
      .getOrCreate()
    val a =sparkSession_tw.sparkContext.textFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\sample.txt")
//    val a =sparkSession_tw.sparkContext.textFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle") //same as above
//    val a = sc.textFile(".//data//twinkle//sample.txt")
    // a: org.apache.spark.rdd.RDD[String]
    println("***** line of sample is : "+a.count)
    // 5
    val a_twinkle = a.filter(_.contains("twinkle"))
    // a_twinkle: org.apache.spark.rdd.RDD[String]

    println("****** line number with filtered with word twinkle: "+a_twinkle.count)
    // 2
    println("******************print each line ******************")
    a_twinkle.collect.foreach(println)
    // twinkle twinkle little star
    // twinkle twinkle little star
  }//end of main
}
