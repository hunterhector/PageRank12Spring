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

    val iter = 10

    //these are global to all methods
    val searchScores = getSearchScores()
    val tMatrix = getTransitionMatrix()

    val userQueryPairs = searchScores.keys

    val N = tMatrix.cols

    val pVector = DenseVector.fill[Double](N)(1.0 / N)
    val gpr = new PageRankPowerMethod(tMatrix, pVector, 0.15, "gpr") //1-alpha is 0.85
    val gpRanks = gpr.getResults(iter, 0.01, false).sortBy(_._1)

    writeRanks("GPR-10",gpRanks)

    val dReader = new DistributionReader()
    val qDistro = dReader.fromFile(new File("data/query-topic-distro.txt")) //(uid,qid)
    val uDistro = dReader.fromFile(new File("data/user-topic-distro.txt")) //(uid,qid)

    val topicVectors = buildTopicalPreferenceVectors(N)

    val topicalRanks = topicVectors.map {
      case (topic, topicVector) => {
        val tpr = new PageRankPowerMethod(tMatrix, topicVector, 0.15, "tpr")
        topic -> tpr.getResults(iter, 0.01, false).sortBy(_._1)
      }
    }

    val u2q1Qtspr = new Array[(Int, Double)](N)
    val u2q1Ptspr = new Array[(Int, Double)](N)

    val tsprWithPtspr = userQueryPairs.foldLeft((new Array[(Int, Double)](N), new Array[(Int, Double)](N))) {
      case ((combinedTopicalRanks, combinedPersonalRanks), (uid, qid)) => {
        topicalRanks.foreach {
          case (topic, topicalRank) => topicalRank.foreach {
            case (docId, rank) => {
              //accumulate topical results
              val oldTopicTuple = combinedTopicalRanks(docId - 1)
              val topicContribution = rank * qDistro(uid)(qid)(topic)
              val topicalCombinedRank = if (oldTopicTuple != null) oldTopicTuple._2 + topicContribution else topicContribution

              //accumulate personal results
              val oldPersonTuple = combinedPersonalRanks(docId - 1)
              val personalContribution = rank * uDistro(uid)(qid)(topic)
              val personalCombinedRank = if (oldPersonTuple != null) oldPersonTuple._2 + personalContribution else personalContribution


              if (uid == 2 && qid == 1){
                u2q1Qtspr(docId - 1) = (docId,topicContribution)
                u2q1Ptspr(docId - 1) = (docId,personalContribution)
              }

              combinedTopicalRanks(docId - 1) = (docId, topicalCombinedRank)
              combinedPersonalRanks(docId - 1) = (docId, personalCombinedRank)
            }
          }
        }
        (combinedTopicalRanks, combinedPersonalRanks)
      }
    }

    writeRanks("QTSPR-U2Q1-10",u2q1Qtspr)
    writeRanks("PTSPR-U2Q1-10",u2q1Ptspr)

    val tsprRankScores = tsprWithPtspr._1
    val qtsprRankScores = tsprWithPtspr._2

    val allRanks = List((gpRanks, "gpr"), (tsprRankScores, "tspr"), (qtsprRankScores, "qtspr"))

    val weightings = List(new NoSearchWeighting(), new LinearWeighting(0.1))

    //compute result for different ranks and different weights
    allRanks.foreach {
      case (ranks, rankName) => {
        val rankMap = ranks.foldLeft(Map[Int, Double]()) {
          case (docId2Rank, (docId, rank)) => {
            docId2Rank + (docId -> rank)
          }
        }
        weightings.foreach(weighting => {
          run(rankMap, weighting, searchScores, rankName + "_" + weighting.name)
        })
      }
    }
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

  def writeRanks(filename:String, ranks:Array[(Int,Double)]) = {
     val pw = new PrintWriter(new File("sample/zhengzhl-"+ filename+".txt"))
     try{
       ranks.foreach{case (docId,rank)=>{
         pw.write(docId+"-"+rank+"\n")
       }}
     }finally {
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