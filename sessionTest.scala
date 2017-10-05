import org.apache.spark.sql.SparkSession

object sessionTest {
  def main(args: Array[String]): Unit = {
    val spark: SparkSession = SparkSession.builder.master("local").getOrCreate
    val sc = spark.sparkContext // Just used to create test RDDs

    val rdd = sc.parallelize(
      Seq(
        ("first", Array(2.0, 1.0, 2.1, 5.4)),
        ("test", Array(1.5, 0.5, 0.9, 3.7)),
        ("choose", Array(8.0, 2.9, 9.1, 2.5))
      )
    )

    val dfWithoutSchema = spark.createDataFrame(rdd)

    dfWithoutSchema.show()
  }

}
