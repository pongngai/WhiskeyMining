/*
option.scala

 */

object OptionBisect {

  //find root option with epsilon
  def findRootEpOpt(low: Double, high: Double)(f: Double => Double): Option[Double] = {
    @annotation.tailrec
    def findR(low: Double, flow: Double, high:Double, fhigh: Double, esp: Double = 1.0e-8): Option[Double] = {
      val mid = (low + high) / 2
      if(math.abs(high - low) < esp){
        Some(mid)
      }else{
        val fmid = f(mid)
        if(fmid == 0) Some(mid)
        else if(flow * fmid < 0) findR(low, flow, mid, fmid)
        else findR(mid, fmid, high, fhigh)
      }
    }
    assert(low < high)
    val flow = f(low)
    val fhigh = f(high)
    if(flow * fhigh >= 0.0) None
    else findR(low, flow, high, fhigh, 1.0e-8)
  }

  //find root option without epsilon
  def findRootOpt(low: Double, high: Double)(f: Double => Double): Option[Double] = {
    @annotation.tailrec
    def findR(low: Double, flow: Double, high:Double, fhigh: Double): Option[Double] = {
      val mid = (low + high) / 2
      val fmid = f(mid)
      if(fmid == 0) Some(mid)
      else if(flow * fmid < 0) findR(low, flow, mid, fmid)
      else findR(mid, fmid, high, fhigh)
    }
    assert(low < high)
    val flow = f(low)
    val fhigh = f(high)
    if(flow * fhigh >= 0.0) None
    else findR(low, flow, high, fhigh)
  }

  def solveQuad(a: Double): Option[Double] = {
    Some(0.0)
  }

  def main(args: Array[String]): Unit = {
    println(findRootOpt(-5.0,10.0)(x => 2.0-x))
    println(findRootOpt(-2.0,0.0)(x => (x + 1.0)*(x - 1.0)))
  }


}

/* eof */

