/*
bisect.scala

 */

object Bisect {

  //with epsilon
  def findRootEp(low: Double, high: Double)(f: Double => Double): Double = {
    @annotation.tailrec
    def findR(low: Double, flow: Double, high:Double, fhigh: Double, esp: Double = 1.0e-8): Double = {
      val mid = (low + high) / 2
      if(math.abs(high - low) < esp){
        mid
      }else{
        val fmid = f(mid)
        if(fmid == 0) mid
        else if(flow * fmid < 0) findR(low, flow, mid, fmid)
        else findR(mid, fmid, high, fhigh)
      }
    }
    assert(low < high)
    val flow = f(low)
    val fhigh = f(high)
    assert(flow * fhigh < 0.0)
    findR(low, flow, high, fhigh, 1.0e-8)
  }

  //without epsilon
  def findRoot(low: Double, high: Double)(f: Double => Double): Double = {
    @annotation.tailrec
    def findR(low: Double, flow: Double, high:Double, fhigh: Double): Double = {
      val mid = (low + high) / 2
      val fmid = f(mid)
      if(fmid == 0) mid
      else if(flow * fmid < 0) findR(low, flow, mid, fmid)
      else findR(mid, fmid, high, fhigh)
    }
    assert(low < high)
    val flow = f(low)
    val fhigh = f(high)
    assert(flow * fhigh < 0.0)
    findR(low, flow, high, fhigh)
  }

  def main(args: Array[String]): Unit = {
    println(findRoot(-0.5, 10.0)(x => 2.0 - x)) 
  }

}

/* eof */

