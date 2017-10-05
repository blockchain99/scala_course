import org.apache.spark.sql.SparkSession

object tw_DataFrame {

  def main(args: Array[String]): Unit = {
    val sparkSession_tw = SparkSession.builder
      .master("local")
      .appName("Twinkle line")
      .getOrCreate()
    val c =sparkSession_tw.read.text("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\sample.txt")

    println("************* c.printSchema : *****")
    c.printSchema
    println("------------------------------------")
    c.show(false)
    println("*** c.count : "+c.count)
    println("^^^^^^^^^^^^   value like '%twinkle%' ^^^^^^^^^^^^^^^^")
    val c_twinkle = c.filter("value like '%twinkle%'")
    c_twinkle.show(false)
  }

}
