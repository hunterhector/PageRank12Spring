package pr.io

import org.eintr.loglady.Logging
import scala.io.Source
import java.io.File

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 3:19 PM
 */

/**
 * A reader that read the distribution file
 * This reader can read both the two distribution files
 */
class DistributionReader extends Logging{

  /**
   * Read from the provided file
   * @param userTopicDistroFile
   * @return  Multi layer map, first key is uid, second key is qid, last map is the distribution by topic
   */
    def fromFile(userTopicDistroFile:File) = {
        Source.fromFile(userTopicDistroFile).getLines().foldLeft(Map[Int,Map[Int,Map[Int,Double]]]())(
          (user2Topic, line)=>{
            val parts = line.split(" ")
            val uid = parts(0).toInt
            val qid = parts(1).toInt

            val topicProb = parts.slice(2,parts.length).foldLeft(Map[Int,Double]())(
              (q2Topic,tuple) =>{
                val topicProb = tuple.split(":")

                q2Topic + (topicProb(0).toInt -> topicProb(1).toDouble)
              }
            )

            val query2TopicProb = user2Topic.getOrElse(uid,Map[Int,Map[Int,Double]]())
            user2Topic + (uid -> (query2TopicProb + (qid -> topicProb)))
          }
        )
    }
}

object DistributionReader{
  def main(args:Array[String]){
    val reader = new DistributionReader()
    val userTopicDistro = reader.fromFile(new File("data/user-topic-distro.txt"))
    println(userTopicDistro.get(6))
  }
}
