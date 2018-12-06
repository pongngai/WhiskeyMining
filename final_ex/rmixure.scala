import breeze.stats.distributions._
import breeze.plot._

class BinomialWithPoissonAndBeta(val poissonMean: Double, val betaA: Double, val betaB: Double) extends Rand[Int]{

    val poisson = new Poisson(poissonMean)
    val beta = new Beta(betaA, betaB)

    def draw(): Int = {
      (for{
        p <- poisson
        b <- beta
        r <- Binomial(p, b)
      } yield r).draw
    } 

    def iterator(): Iterator[Int] = {
      Iterator.continually(draw())
    }

    def stream(): Stream[Int] = {
      Stream.continually(draw())
    }

    def stream(n: Int): Stream[Int] = {
      Stream.fill(n)(draw())
    }

}

object Simple{

  def main(args: Array[String]):Unit = {
    val mix = new BinomialWithPoissonAndBeta(20.0, 4.0, 4.0)
    val samples = mix.stream(10000).toList
    val mean = samples.sum / samples.size
    println(mean)
    //val fig = Figure("Distribution")
    //fig.width = 1000
    //fig.height = 800
    //val p1 = fig.subplot(0)
    //p1 += hist(samples, 50)
    //p1.xlim = (-5, 30)
   // p1.xlabel = "y"
    //p1.title("Distribution of observed response")
    //fig.refresh
  }
}

