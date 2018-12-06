

import breeze.linalg._

object GLM {

@annotation.tailrec
def IRLS(
  b: Double => Double,
  bp: Double => Double,
  bpp: Double => Double,
  y: DenseVector[Double],
  X: DenseMatrix[Double],
  bhat0: DenseVector[Double],
  its: Int,
  tol: Double = 0.0000001
): DenseVector[Double] = if (its == 0) {
  println("WARNING: IRLS did not converge")
  bhat0
} else {
  val eta = X * bhat0
  val W = eta map bpp
  val z = y - (eta map bp)
  val bhat = bhat0 + (X.t(*,::) * W * X) \ (X.t * z)
  if (norm(bhat-bhat0) < tol) bhat else
    IRLS(b,bp,bpp,y,X,bhat,its-1,tol)
}

def logReg(
  y: DenseVector[Double],
  X: DenseMatrix[Double],
  its: Int = 30
): DenseVector[Double] = {
  def expit(x: Double): Double = 1.0/(1.0+math.exp(-x))
  def b(x: Double): Double = math.log(1.0+math.exp(x))
  def bp(x: Double): Double = expit(x)
  def bpp(x: Double): Double = {
    val e = math.exp(-x)
      e/((1.0+e)*(1.0+e))
  }
  val bhat0 = DenseVector.zeros[Double](X.cols)
  IRLS(b,bp,bpp,y,X,bhat0,its)
}


def poiReg(
  y: DenseVector[Double],
  X: DenseMatrix[Double],
  its: Int = 30
): DenseVector[Double] = {
  val bhat0 = DenseVector.zeros[Double](X.cols)
  IRLS(math.exp,math.exp,math.exp,y,X,bhat0,its)
}


}
