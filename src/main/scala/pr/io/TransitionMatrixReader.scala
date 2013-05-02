package pr.io

import org.eintr.loglady.Logging
import java.io.File
import breeze.linalg.CSCMatrix
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 4/30/13
 * Time: 10:43 PM
 */

/**
 * This reader directly read the transposed version of the transition matrix
 * which means, each cell (i,j), now represent the percentage of weight from j to i
 * Our matrix is zero based, and the data is zero based, so we minus one for each index
 */
class TransitionMatrixReader() extends Logging {

  def fromFile(transitionFile: File) = {
    val allPairsAndMax = Source.fromFile(transitionFile).getLines().foldLeft((List[(Int, Int)](), Map[Int, Int](), 0)) {
      case ((oldlist, outdegrees, oldmax), line) => {
        val parts = line.split(" ")
        val from = parts(0).toInt-1
        val to = parts(1).toInt-1
        val tuple = (from, to)
        val oldOutDegree = outdegrees.getOrElse(from, 0)

        (tuple :: oldlist, outdegrees + (from -> (oldOutDegree + 1)), from max to max oldmax)
      }
    }

    val allPairs = allPairsAndMax._1
    val outdegrees = allPairsAndMax._2
    val numberOfPages = allPairsAndMax._3 + 1

    val builder = new CSCMatrix.Builder[Double](numberOfPages, numberOfPages)

    allPairs.foreach {
      case (from, to) => {
        val weight = 1.0 / outdegrees(from)
        builder.add(to, from, weight) //NOTE: here already transpose
      }
    }
    builder.result()
  }
}

object TransitionMatrixReader {
  def main(args:Array[String]){
    //just test a little bit
    val reader = new TransitionMatrixReader()
    val tMatrix = reader.fromFile(new File("data/sample.txt"))

    println ("Read a matrix of size : %d x %d".format(tMatrix.rows,tMatrix.cols))

    val colPtrs = tMatrix.colPtrs
    val tData = tMatrix.data
    val tRows = tMatrix.rowIndices

    val colRanges = colPtrs.slice(0,tMatrix.cols).zip(colPtrs.slice(1,tMatrix.cols+1))

    var record = 0
    colRanges.zipWithIndex.foreach{case((start,end),idx) =>{
      println("Column # : %d".format(idx))
      tData.slice(start,end).foreach(t=>print(t))
      println()
      println("Hope you are stochastic : %f".format(tData.slice(start,end).sum))
      println("Check out degree : %d".format(end-start))
      record += (end-start)
    }}

    println("Record read : %d".format(record))
  }
}