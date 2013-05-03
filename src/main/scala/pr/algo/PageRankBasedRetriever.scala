package pr.algo

import org.eintr.loglady.Logging
import pr.model.TrecLikeResult
import pr.io.{DistributionReader, DocumentClassReader, TransitionMatrixReader, SearchScoreReader}
import java.io.{PrintWriter, File}
import breeze.linalg.DenseVector

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:53 PM
 */
class PageRankBasedRetriever() extends Logging {

  def retreive(ws: WeightingScheme, searchScoresByQuery: Map[(Int, Int), List[(Int, Double)]], rankScores: Map[Int, Double], runId: String) = {
    searchScoresByQuery.foldLeft(List[TrecLikeResult]()) {
      case (results, ((uid, qid), searchScores)) => {
        val mergedScores = searchScores.map {
          case (docId, searchScore) => {
            (docId, ws.getWeightedScore(rankScores(docId), searchScore))
          }
        }.toList.sortBy(_._2).reverse.zipWithIndex.map {
          case ((docId, weightedScore), pos) => {
            new TrecLikeResult(uid.toString + "-" + qid.toString, docId, pos + 1, weightedScore, runId)
          }
        }

        println(mergedScores.maxBy(t => t.score))

        results ::: mergedScores
      }
    }
  }
}

object PageRankBasedRetriever extends Logging {

  def main(args: Array[String]) {
    log.info("Preparing page rank")

    //these are global to all methods
    val searchScores = getSearchScores()
    val tMatrix = getTransitionMatrix()

    val userQueryPairs = searchScores.keys

    val N = tMatrix.cols

    val pVector = DenseVector.fill[Double](N)(1.0 / N)
    //    val gpr = new PageRankPowerMethod(tMatrix, pVector, 0.15, "gpr") //1-alpha is 0.85
    //    val gpRanks = gpr.getResults(20, 0.01, false)

    val dReader = new DistributionReader()
    val qDistro = dReader.fromFile(new File("data/query-topic-distro.txt")) //(uid,qid)
    val uDistro = dReader.fromFile(new File("data/user-topic-distro.txt")) //(uid,qid)

    val topicVectors = buildTopicalPreferenceVectors(N)

    val topicalRanks = topicVectors.map {
      case (topic, topicVector) => {
        val tpr = new PageRankPowerMethod(tMatrix, topicVector, 0.15, "tpr")
        topic -> tpr.getResults(20, 0.01, false).sortBy(_._1)
      }
    }

    val tsprRankScores = userQueryPairs.foldLeft(new Array[(Int, Double)](N)) {
      case (combinedTopicalRanks, (uid, qid)) => {
        topicalRanks.foreach {
          case (topic, topicalRank) => topicalRank.foreach {
            case (docId, rank) => {
              val oldTuple = combinedTopicalRanks(docId - 1)
              val topicContribution = rank * qDistro(uid)(qid)(topic)
              val combinedRank = if (oldTuple == null) oldTuple._2 + topicContribution else topicContribution
              combinedTopicalRanks(docId - 1) = (docId, combinedRank)
            }
          }
        }
        combinedTopicalRanks
      }
    }

    val allRanks = List(tsprRankScores)

    val weightings = List(new NoSearchWeighting())

    //compute result for different ranks and different weights
    allRanks.foreach(ranks => {
      val rankMap = ranks.foldLeft(Map[Int, Double]()) {
        case (docId2Rank, (docId, rank)) => {
          docId2Rank + (docId -> rank)
        }
      }
      weightings.foreach(weighting => {
        run(rankMap, weighting, searchScores, "tpr" + weighting.name)
      })
    })
  }

  def buildTopicalPreferenceVectors(N: Int) = {
    val topicReader = new DocumentClassReader()
    val docTopics = topicReader.fromFile(new File("data/doc_topics.txt"))
    docTopics.foldLeft(Map[Int, DenseVector[Double]]()) {
      case (topicalVectors, (topic, docList)) => {
        val topicV = DenseVector.fill[Double](N)(0.0)
        docList.foreach(docId => {
          topicV(docId - 1) = 1.0 / docList.length
        })
        topicalVectors + (topic -> topicV)
      }
    }
  }

  def run(rankMap: Map[Int, Double], weightingScheme: WeightingScheme, searchScores: Map[(Int, Int), List[(Int, Double)]], runId: String) {
    log.info("Running %s".format(runId))

    val r = new PageRankBasedRetriever()
    val results = r.retreive(weightingScheme, searchScores, rankMap, runId)

    val pw = new PrintWriter(new File("output/" + runId))
    try {
      pw.write(TrecLikeResult.header + "\n")
      results.foreach(res => {
        pw.write(res.toString + "\n")
      })
    } finally {
      pw.close()
    }
  }

  def getSearchScores() = {
    val scoreReader = new SearchScoreReader()
    scoreReader.fromDirectory(new File("data/indri-lists"))
  }

  def getTransitionMatrix() = {
    val tReader = new TransitionMatrixReader()
    tReader.fromFile(new File("data/transition.txt"), 1)
  }
}