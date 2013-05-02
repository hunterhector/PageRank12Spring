package pr.algo

import org.eintr.loglady.Logging
import pr.model.TrecLikeResult
import pr.io.{TransitionMatrixReader, SearchScoreReader}
import java.io.File
import breeze.linalg.CSCMatrix

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:53 PM
 */
class Retriever extends Logging {
  def retreive(ws: WeightingScheme, searchScores: Map[(Int, Int), Map[Int, Double]], pr: PageRankPowerMethod) = {
    val ranks = pr.stepUntil(20,0.01)
    searchScores.foldLeft(List[TrecLikeResult]()){
      case (results,((uid,qid),scoreList)) =>{
         scoreList()
      }
    }
  }
}

object Retriever {
  val r = new Retriever()

  val scoreReader = new SearchScoreReader()
  val query2Scores = scoreReader.fromDirectory(new File("data/indri-lists"))
  val tReader = new TransitionMatrixReader()
  val tMatrix = tReader.fromFile(new File("data/transition.txt"))

  val N = tMatrix.cols
  val preferenceVectorBuilder = new CSCMatrix.Builder[Double](N, 1)

  for (i <- 0 until N) {
    preferenceVectorBuilder.add(i, 0, 1.0 / N)
  }

  val pVector = preferenceVectorBuilder.result
  val pr = new PageRankPowerMethod(tMatrix, pVector, 0.15) //1-alpha is 0.85

  r.retreive(new NoSearchWeighting(), query2Scores, pr)
}
