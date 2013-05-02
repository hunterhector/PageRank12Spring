package pr.io

import org.eintr.loglady.Logging
import java.io.File
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:35 PM
 */
class SearchScoreReader extends Logging {
  def fromDirectory(scoreDir: File) = {
    if (!scoreDir.isDirectory) {
      throw new IllegalArgumentException("Not a directory")
    }

    val resultFiles = scoreDir.listFiles()

    resultFiles.foldLeft(Map[(Int,Int),Map[Int,Double]]())((query2Scores,resultFile)=>{
       val idPart = resultFile.getName.split(".")(0)
       val ids = idPart.split("-")
       val uid = ids(0).toInt
       val qid = ids(1).toInt

      query2Scores + ((uid,qid) -> parseScores(resultFile))
    })
  }

  private def parseScores(scoreFile:File):Map[Int,Double]={
    Source.fromFile(scoreFile).getLines().foldLeft(Map[Int,Double]())((scoreMap,line)=>{
      val parts = line.split(" ")
      val docId = parts(2).toInt
      val score = parts(4).toDouble
      scoreMap + (docId -> score)
    })
  }
}
