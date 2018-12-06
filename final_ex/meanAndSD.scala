import breeze.stats.distributions._

object myapp{

    def main(args: Array[String]): Unit = {
        val v = Vector.fill(10000)(Uniform(0, 1).sample)
        println(meanAndSD(v))
    }
    
    def meanAndSD(x: Vector[Double]): (Double, Double) = {
	    val mean = x.sum / x.size
	    val upper = x.map(x => math.pow((x - mean), 2)).sum
	    val std = math.sqrt(upper / x.size)
	    (mean, std)
    }
}
