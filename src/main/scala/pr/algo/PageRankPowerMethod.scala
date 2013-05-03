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
/**
 * Main class that run tha pagerank method
 * @param transitionMatrix  The transition matrix of network
 * @param preferenceVector  The preference vector
 * @param alpha  The dampen factor
 * @param name  The name of this method, used for logging
 */
class PageRankPowerMethod(transitionMatrix: CSCMatrix[Double], preferenceVector: DenseVector[Double], alpha: Double, val name:String)extends Logging {
  val N = transitionMatrix.cols

  val rankVectorBuilder = new CSCMatrix.Builder[Double](N, 1)

  for (i <- 0 until N) {
    rankVectorBuilder.add(i, 0, 1.0 / N)
  }

  //construct a uniform start vector
  val rankVector = rankVectorBuilder.result

  val beta = 0.1

  val teleportVectorBuilder = new CSCMatrix.Builder[Double](N,1)

  for (i <- 0 until N){
    teleportVectorBuilder.add(i,0,alpha*(beta/N + (1-beta)*preferenceVector(i)))
  }

  //construct the teleport vector by combining preference with uniform vector
  val teleportVector = teleportVectorBuilder.result()

  //scaled transition matrix by 1-alpha
  val scaledTransitionMatrix = elementWiseProduct(transitionMatrix,1-alpha)

  /**
   * Implement a element wise produce for SparseMatrix
   * @param m the input matrix
   * @param a the scalar
   * @return  the element-wise produce of matrix and scalar
   */
  def elementWiseProduct(m:CSCMatrix[Double],a:Double) ={
      val builder = new CSCMatrix.Builder[Double](m.rows, m.cols)
      m.activeIterator.foreach{case ((r,c),v)=>{
        builder.add(r,c,a*v)
      }}
      builder.result()
  }

  /**
   * Implement sum of two matrix
   * @param m  first matrix
   * @param n  second matrix
   * @return  Sum of the two matrices
   */
  def elementWiseSum(m:CSCMatrix[Double],n:CSCMatrix[Double]) = {
    if (m.cols != n.cols || m.rows != n.rows){
      throw new IllegalArgumentException("Wrong matrix indices!")
      println(m.cols+" "+m.rows+" "+n.cols+" "+n.rows)
    }
    val builder = new CSCMatrix.Builder[Double](m.rows, m.cols)

    m.iterator.zip(n.iterator).foreach{case (((rm,cm),vm),((rn,cn),vn)) =>{

      builder.add(rm,cm,vm+vn)
    }}
    builder.result()
  }

  /**
   * Return the result an array of DocID, Rank pair
   * @param maxIter   iteration stop criteria
   * @param minDelta  delata stop criteria
   * @param sorted  whether sort the result by rank
   * @return  an array of (DocId, Rank)
   */
  def getResults(maxIter:Int, minDelta: Double, sorted:Boolean) = {
    val ranks = stepUntil(maxIter,minDelta)

    val rankWithDocId = ranks.zipWithIndex.map{case (rank,idx) => (idx+1,rank)}

    if (sorted)
      rankWithDocId.sortBy(_._2).reverse
    else
      rankWithDocId
  }

  /**
   * One step is simply one multiplicaiton
   * @param ranks
   * @return
   */
  def step(ranks:CSCMatrix[Double]) = {
    elementWiseSum(scaledTransitionMatrix * ranks , teleportVector)
  }

  /**
   * Control the stopping
   * @param maxIter
   * @param minDelta
   * @return
   */
  def stepUntil(maxIter:Int,minDelta:Double) = {
    var oldRank = rankVector
    for (i <- 0 until maxIter) {
      log.debug("Iteration : %d".format(i))
      val newRank = step(oldRank)
      oldRank = newRank
    }

    (oldRank.toDenseMatrix.toDenseVector).toArray
  }
}

object PageRankPowerMethod {
  def main(args: Array[String]) {
    val tReader = new TransitionMatrixReader()
    val tMatrix = tReader.fromFile(new File("data/transition.txt"),1)

    val N = tMatrix.cols
    val pVector = DenseVector.fill[Double](N)(1.0/N)
    val pr = new PageRankPowerMethod(tMatrix, pVector, 0.15,"gpr") //1-alpha is 0.85
    val ranks = pr.getResults(100,0.01,false)

    println("Total %d results retrieved".format(ranks.length))
  }
}