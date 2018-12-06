object LogRegBySpark {

  import org.apache.spark
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.DataFrameReader
  import org.apache.spark.sql.{Dataset, Row, DataFrame}

  import org.apache.spark.SparkContext
  import org.apache.spark.SparkContext._
  import org.apache.spark.SparkConf

  import org.apache.spark.sql.functions._

  import collection.mutable.{ArrayBuffer, ListBuffer}

  def readData(fileName: String, sep: String,
    header: Boolean = true, inferSchema: Boolean = true) = {

  // see Lecture-note pp. 135 and 141.

    import org.apache.log4j.Logger
    import org.apache.log4j.Level

    Logger.getLogger("org").setLevel(Level.ERROR)
    Logger.getLogger("akka").setLevel(Level.ERROR)
    Logger.getRootLogger.setLevel(Level.ERROR)
    Logger.getRootLogger.setLevel(Level.FATAL)

    val spark = SparkSession.builder().
      master("local").
      appName("spark-shell").
      getOrCreate()

    Logger.getRootLogger.setLevel(Level.FATAL)

    // For implicit conversions like converting RDDs to DataFrames
    import spark.implicits._

    val sc = spark.sparkContext

    def df = spark.read.
               option("sep", sep).
               option("header", header).
               option("inferSchema", inferSchema).
               csv(fileName)

    (spark, sc, df)
  }


  def dfToDFLogR(df: org.apache.spark.sql.DataFrame): org.apache.spark.sql.DataFrame = {

  // see Lecture-note pp. 140, 142 and 144.

    val dfLogR = df.toDF("c0","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12","c13","c14","c15","c16","c17","c18","c19","c20","c21","c22","c23","c24","c25","c26","c27","c28","c29","c30","c31","c32","c33","c34","c35","c36","c37","c38","c39","c40","c41","c42","c43","c44","c45","c46","c47","c48","c49","c50","c51","c52","c53","c54","c55","c56","c57")			//.persist

    import org.apache.spark.ml.feature.RFormula
    import org.apache.spark.ml.feature.RFormulaModel
 
    val dfLogR2 = new RFormula().setFormula("c57 ~ c0+c1+c2+c3+c4+c5+c6+c7+c8+c9+c10+c11+c12+c13+c14+c15+c16+c17+c18+c19+c20+c21+c22+c23+c24+c25+c26+c27+c28+c29+c30+c31+c32+c33+c34+c35+c36+c37+c38+c39+c40+c41+c42+c43+c44+c45+c46+c47+c48+c49+c50+c51+c52+c53+c54+c55+c56").fit(dfLogR).transform(dfLogR).select("label","features")

    dfLogR2

   /* end of your addition */
  }

  import org.apache.spark.ml.classification._
  import org.apache.spark.ml.classification.{LogisticRegression, LogisticRegressionModel}
  import org.apache.spark.ml.tuning.{CrossValidator, CrossValidatorModel, ParamGridBuilder}

  // regParam * ( a L1 + (1-a) L2 )
  // a (elasticNetParam) = 1 for L1, 0 for L2, and (0.1) for a L1 + (1-a) L2


  def logisticRegression( standardization: Boolean = false, 
    elasticNetParam: Double = 0.0, regParam: Double = 0.1, maxIter: Int = 100):
	LogisticRegression  = {

  // see Lecture-note pp. 144.

    import org.apache.spark.ml.classification._
   
    val lr  = new LogisticRegression
    lr.setStandardization(standardization)
    lr.setElasticNetParam(elasticNetParam)
    lr.setRegParam(regParam)
    lr.setMaxIter(maxIter)
 
   lr
   /* end of your addition */
  }

  def searchBestHyperParam(logReg: LogisticRegression, dflogr: DataFrame, nFold: Int = 3):
	 Tuple4[CrossValidatorModel, Double, LogisticRegression, LogisticRegressionModel]= {

  // see Lecture-note pp. 145, and 146.

    import breeze.linalg.linspace
    val lambdas = linspace(-12,4,60).toArray.map{math.exp(_)}

    import org.apache.spark.ml.tuning._
    import org.apache.spark.ml.evaluation._
    
    val paramGrid = new ParamGridBuilder().addGrid(logReg.regParam,lambdas).build()
    
    val cv = new CrossValidator().setEstimator(logReg).setEstimator(logReg).setEvaluator(new BinaryClassificationEvaluator).setEstimatorParamMaps(paramGrid).setNumFolds(nFold)
    val cvMod = cv.fit(dflogr)

    val logrfit = logReg.fit(dflogr)

    val lambda  = cvMod.bestModel.extractParamMap.getOrElse(cvMod.bestModel.getParam("regParam"),0.0).asInstanceOf[Double]
   
   
   (cvMod,lambda,logReg,logrfit)
   /* end of your addition */
  }


  def main(args: Array[String]): Unit = {

    //import LogRegBySpark._

    val (spark, sc, df) = readData("spambase.data", sep = ",", header = false, inferSchema = true )

    import spark.implicits._

    val dfLogR = dfToDFLogR(df)

    val weights = Array(0.75, 0.25)
    val datasets = dfLogR.randomSplit(weights)
    val trainDF = datasets(0)
    val testDF = datasets(1)

    val logReg = logisticRegression( standardization = false, 
                   elasticNetParam = 1.0, regParam = 8.058254732499574E-6, maxIter = 100)

    val (cvMod, lambda, bestLogReg, bestLogRegFit) = 
      searchBestHyperParam(logReg, trainDF, nFold = 3)

    //println( cvMod.explainParams )
    println( cvMod.extractParamMap )

    //println( cvMod.bestModel.explainParams )
    println( cvMod.bestModel.extractParamMap )
    println(s"Best lambda = $lambda")

    val trainingSummary = bestLogRegFit.binarySummary

    // Obtain the objective per iteration.
    val objectiveHistory = trainingSummary.objectiveHistory
    println("objectiveHistory:")
    objectiveHistory.foreach(loss => println(loss))

    //
    println("Coefficients and intercept with the best lambda")
    println(s"Intercept: ${bestLogRegFit.intercept}")
    bestLogRegFit.coefficients.foreachActive{(i, x ) => println(i, x) }
    val nZeros = bestLogRegFit.coefficients.toArray.map{x  => if(x == 0) 1 else 0 }.sum
    println( s"#featuers whose coeeficient is eqaul to 0.0: ${nZeros}")
    
    //
    val accuracy = trainingSummary.accuracy
    println(s"Accuracy: $accuracy")
    //Accuracy: 0.934030804998547

    println( bestLogRegFit.extractParamMap )

    // Test on testDF
    println("Test on testDF") 

    println(s"Accuracy: ${bestLogRegFit.evaluate(testDF).accuracy}")

  /*  The followings are commented out because they take too much time.

    // Obtain the receiver-operating characteristic as a dataframe and areaUnderROC.
    val roc = trainingSummary.roc
    println( roc.show() )
    println(s"areaUnderROC: ${trainingSummary.areaUnderROC}")

    // Set the model threshold to maximize F-Measure
    val fMeasure = trainingSummary.fMeasureByThreshold
    //val maxFMeasure = fMeasure.select(max("F-Measure")).head().getDouble(0)

    // spark.implict._ must be imported.
    val maxFMeasure = fMeasure.select("F-Measure").map {x => (x.getDouble(0)) }.reduce{ _.max(_) }
    // If spark.implict._  is not imported,
    //val maxFMeasure = fMeasure.select("F-Measure").collect.map {x => (x.getDouble(0)) }.reduce{ _.max(_) }
    //maxFMeasure: Double = 0.9147457006952068

    val bestThreshold = fMeasure.where($"F-Measure" === maxFMeasure).
      select("threshold").head().getDouble(0)
    //bestThreshold: Double = 0.43254698170435485

    bestLogRegFit.setThreshold(bestThreshold)

    println( bestLogRegFit.extractParamMap )

    // Test on testDF
    println("Test on testDF") 

    val logRegSummary = bestLogRegFit.evaluate(testDF)

    println(s"Accuracy: ${logRegSummary.accuracy}")

 */

    spark.stop()

  }


}
