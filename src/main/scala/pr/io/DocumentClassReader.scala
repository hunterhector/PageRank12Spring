package pr.io

import java.io.File
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 4:01 PM
 */

/**
 * A reader that reads the document and topic mapping
 */
class DocumentClassReader {

  /**
   * Read from the given file
   * @param docTopicFile
   * @return  a map from topic, to document that it contains
   */
    def fromFile(docTopicFile:File) = {
         Source.fromFile(docTopicFile).getLines().foldLeft(Map[Int,List[Int]]())(((topic2Doc,line) =>{
             val parts = line.split(" ")
             val docId = parts(0).toInt
             val topic = parts(1).toInt
             val docList = topic2Doc.getOrElse(topic,List[Int]())
             val newDocList = docId::docList
             topic2Doc + (topic -> newDocList)
         }))
    }
}
