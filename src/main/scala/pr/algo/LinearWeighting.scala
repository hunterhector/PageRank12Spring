package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:58 PM
 */

/**
 * A simple linear weighted scheme
 * @param w the importance of the pagerank score
 */
class LinearWeighting(w:Double) extends WeightingScheme{
  /**
   * Return a weighted sum of two score
   * @param prScore
   * @param searchScore
   * @return
   */
  def getWeight(prScore: Double, searchScore: Double):Double = {
      return w*prScore + (1-w)*searchScore
  }
}
