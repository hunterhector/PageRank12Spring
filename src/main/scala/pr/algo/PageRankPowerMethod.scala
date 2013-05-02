package pr.algo

import breeze.linalg._
import java.io.File
import pr.io.TransitionMatrixReader
import org.eintr.loglady.Logging

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 4/30/13
 * Time: 9:44 PM
 */
class PageRankPowerMethod(transitionMatrix: CSCMatrix[Double], preferenceVector: CSCMatrix[Double], alpha: Double)extends Logging {
  val N = transitionMatrix.cols

  val rankVectorBuilder = new CSCMatrix.Builder[Double](N, 1)

  for (i <- 0 until N) {
    rankVectorBuilder.add(i, 0, 1.0 / N)
  }

  val rankVector = rankVectorBuilder.result

  val builder = new VectorBuilder[Double](N,0)

  for ( i <- 0 until N){
    builder.add(i,0.0)
  }

  val scaledPreferenceVector = elementWiseProd(preferenceVector, alpha)

  val scaledTransitionMatrix = elementWiseProd(transitionMatrix, (1 - alpha))

  def step(oldRank: CSCMatrix[Double]) = {
    val mul = scaledTransitionMatrix * oldRank
    val nextRank = elementWiseMatrixSum(mul, scaledPreferenceVector)
    nextRank
  }

  def stepUntil(maxIter:Int,minDelta:Double) = {
    var oldRank = rankVector
    for (i <- 0 until maxIter) {
      log.debug("Iteration : %d".format(i))
//      println(oldRank)
      val newRank = step(oldRank)
      oldRank = newRank
    }

    val colPtrs = oldRank.colPtrs
    val start = colPtrs(0)
    val end = colPtrs(1)

    val rankArray = oldRank.data.slice(start,end)
    val rowArray = oldRank.rowIndices.slice(start,end).map(r=>r+1)

    rowArray.zip(rankArray).sortBy(_._2).reverse
  }

  def elementWiseProd(m: CSCMatrix[Double], a: Double): CSCMatrix[Double] = {
    val resBuilder = new CSCMatrix.Builder[Double](m.rows, m.cols)
    m.activeIterator.foreach {
      case ((row, col), v) => {
        resBuilder.add(row, col, v * a)
      }
    }

    resBuilder.result()
  }

  def elementWiseMatrixMinus(m: CSCMatrix[Double], n: CSCMatrix[Double]): CSCMatrix[Double] = {
    val mr = m.rows
    val mc = m.cols
    val nr = n.rows
    val nc = n.cols

    if (mr != nr || mc != nc) {
      throw new IllegalArgumentException("Different dimension in matrix")
    }

    val resBuilder = new CSCMatrix.Builder[Double](mr, mc)
    m.activeIterator.zip(n.activeIterator).foreach {
      case (((mRow, mCol), mv), ((nRow, nCol), nv)) => {
        resBuilder.add(mRow, mCol, mv - nv)
      }
    }

    resBuilder.result()
  }

  def elementWiseMatrixSum(m: CSCMatrix[Double], n: CSCMatrix[Double]): CSCMatrix[Double] = {
    val mr = m.rows
    val mc = m.cols
    val nr = n.rows
    val nc = n.cols

    if (mr != nr || mc != nc) {
      throw new IllegalArgumentException("Different dimension in matrix")
    }

    val resBuilder = new CSCMatrix.Builder[Double](mr, mc)
    m.activeIterator.zip(n.activeIterator).foreach {
      case (((mRow, mCol), mv), ((nRow, nCol), nv)) => {
        resBuilder.add(mRow, mCol, mv + nv)
      }
    }

    resBuilder.result()
  }

}

object PageRankPowerMethod {

  def main(args: Array[String]) {

    val tReader = new TransitionMatrixReader()
    val tMatrix = tReader.fromFile(new File("data/transition.txt"))

    val N = tMatrix.cols

    val preferenceVectorBuilder = new CSCMatrix.Builder[Double](N, 1)

    for (i <- 0 until N) {
      preferenceVectorBuilder.add(i, 0, 1.0 / N)
    }

    val pVector = preferenceVectorBuilder.result
    val pr = new PageRankPowerMethod(tMatrix, pVector, 0.15) //1-alpha is 0.85
    val ranks = pr.stepUntil(20,0.01)

    ranks.take(10).foreach(
     r=>println(r)
    )
  }
}
