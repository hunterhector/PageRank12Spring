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
class TransitionMatrixReader(transitionFile:File) extends Logging{
     val allPairs = Source.fromFile(transitionFile).getLines().foldLeft((List[(Int,Int)](),0)){
       case ((oldlist,oldmax), line)=>{

       val parts = line.split(" ")
       val from = parts(0).toInt
       val to = parts(1).toInt
       val tuple = (from,to)

         (tuple :: oldlist, from max to max oldmax)
     }}



     val builder = new CSCMatrix.Builder[Double](0,0)

}
