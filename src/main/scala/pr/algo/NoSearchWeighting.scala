package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:57 PM
 */

/**
 * A dummy weighting scheme that always return the pagerank score
 */
class NoSearchWeighting extends  WeightingScheme{
  def getWeightedScore(prScore: Double, searchScore: Double):Double = {
    return prScore
  }

  def name = "ns"
}
