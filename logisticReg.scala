package solutions

import org.apache.spark.ml.classification.{BinaryLogisticRegressionSummary, LogisticRegression}
import org.apache.spark.ml.evaluation.BinaryClassificationEvaluator
import org.apache.spark.ml.feature.{StringIndexer, VectorAssembler}
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.functions._


/*
 * https://archive.ics.uci.edu/ml/datasets/Breast+Cancer+Wisconsin+(Original)
 * Array(1000025,5,1,1,1,2,1,3,1,1,2)
   0. Sample code number            id number
   1. Clump Thickness               1 - 10
   2. Uniformity of Cell Size       1 - 10
   3. Uniformity of Cell Shape      1 - 10
   4. Marginal Adhesion             1 - 10
   5. Single Epithelial Cell Size   1 - 10
   6. Bare Nuclei                   1 - 10
   7. Bland Chromatin               1 - 10
   8. Normal Nucleoli               1 - 10
   9. Mitoses                       1 - 10
  10. Class:                        (2 for benign, 4 for malignant)
 */

object logisticReg {
  def main(args: Array[String]): Unit = {
//    val conf = new SparkConf().setMaster("local").setAppName("logistic Regression")
//    val sc = new SparkContext(conf)
//    // Load our input data.
//    val input = sc.textFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\sample.txt")

      case class Obs(clas: Double, thickness: Double, size: Double, shape: Double, madh: Double, epsize: Double, bnuc: Double, bchrom: Double, nNuc: Double, mit: Double)

      def parseObs(line: Array[Double]): Obs = {
        Obs(
          if (line(9) == 4.0) 1 else 0, line(0), line(1), line(2), line(3), line(4), line(5), line(6), line(7), line(8)
        )
      }
/*list.drop(num) : drop num elements in list  */
      def parseRDD(rdd: RDD[String]): RDD[Array[Double]] = {
        rdd.map(_.split(",")).filter(_(6) != "?").map(_.drop(1)).map(_.map(_.toDouble))
      }

      def main(args: Array[String]) {

//        val conf = new SparkConf().setAppName("stock")
//        val sc = new SparkContext(conf)
//        val sqlContext = new SQLContext(sc)
//        import sqlContext.implicits._
/****** make ref for SparkSession.builder *********/
       val spark = SparkSession.builder
         .master("local")
         .appName("Stock")
         .getOrCreate()
       import spark.implicits._
//val lines = Seq("This is the first line",
//  "This is the second line",
//  "This is the third line")
//val rdd = sparkSession.sparkContext.parallelize(lines)

        //  val rdd = sc.textFile("wdbc.data")
        val rdd = spark.sparkContext.textFile("C:\\Users\\Gloria\\learning-spark\\data\\twinkle\\wbcd.csv")
        val obsRDD = parseRDD(rdd).map(parseObs)

//        val obsDF = obsRDD.toDF().cache()  //Error: value toDF is not a member org.apache.spark.rddRDD[Obs]
        val obsDF = spark.createDataFrame(obsRDD).cache()
        val featureCols = Array("thickness", "size", "shape", "madh", "epsize", "bnuc", "bchrom", "nNuc", "mit")
        val assembler = new VectorAssembler().setInputCols(featureCols).setOutputCol("features")
//        val df2 = assembler.transform(obsDF)
        val df2 = assembler.transform(obsDF)
/* VectorAssembler is a transformer that combines a given list of columns into a single vector column
. It is useful for combining raw features and features generated by different feature transformers
into a single feature vector, in order to train ML models like logistic regression and decision trees.
VectorAssembler accepts the following input column types: all numeric types, boolean type, and vector type.
//In each row, the values of the input columns will be concatenated into a vector in the specified order.
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.ml.linalg.Vectors

val dataset = spark.createDataFrame(
  Seq((0, 18, 1.0, Vectors.dense(0.0, 10.0, 0.5), 1.0))
).toDF("id", "hour", "mobile", "userFeatures", "clicked")

val assembler = new VectorAssembler()
  .setInputCols(Array("hour", "mobile", "userFeatures"))
  .setOutputCol("features")

val output = assembler.transform(dataset)
println("Assembled columns 'hour', 'mobile', 'userFeatures' to vector column 'features'")
output.select("features", "clicked").show(false)  */

        /* StringIndexer encodes a string column of labels to a column of label indices.
        The indices are in [0, numLabels), ordered by label frequencies,
        so the most frequent label gets index 0. If the input column is numeric,
        we cast it to string and index the string values.   */
        val labelIndexer = new StringIndexer().setInputCol("clas").setOutputCol("label")
        val df3 = labelIndexer.fit(df2).transform(df2)

        val splitSeed = 5043
        val Array(trainingData, testData) = df3.randomSplit(Array(0.7, 0.3), splitSeed)

        val lr = new LogisticRegression().setMaxIter(10).setRegParam(0.3).setElasticNetParam(0.8)
        val model = lr.fit(trainingData)

        val predictions = model.transform(testData)

        predictions.select("clas", "label", "prediction").show(5)

        val trainingSummary = model.summary
        val objectiveHistory = trainingSummary.objectiveHistory
        objectiveHistory.foreach(loss => println(loss))

        val binarySummary = trainingSummary.asInstanceOf[BinaryLogisticRegressionSummary]

        // Obtain the receiver-operating characteristic as a dataframe and areaUnderROC.
        val roc = binarySummary.roc
        roc.show()
        println(binarySummary.areaUnderROC)

        // Set the model threshold to maximize F-Measure
        val fMeasure = binarySummary.fMeasureByThreshold
        val fm = fMeasure.col("F-Measure")
        val maxFMeasure = fMeasure.select(max("F-Measure")).head().getDouble(0)
        val bestThreshold = fMeasure.where($"F-Measure" === maxFMeasure).select("threshold").head().getDouble(0)
        model.setThreshold(bestThreshold)

        val evaluator = new BinaryClassificationEvaluator().setLabelCol("label")

        val accuracy = evaluator.evaluate(predictions)

      }

  }

}
