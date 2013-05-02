package pr.io

import java.io.File
import scala.io.Source

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 4:01 PM
 */
class DocumentClassReader {
    def fromFile(docTopicFile:File){
         Source.fromFile(docTopicFile).getLines().foldLeft(Map[Int,Int]())(((doc2Topic,line) =>{
             val parts = line.split(" ")
             doc2Topic + (parts(0).toInt -> parts(1).toInt)
         }))
    }
}
