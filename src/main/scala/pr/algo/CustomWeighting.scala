package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/2/13
 * Time: 11:48 PM
 */
class CustomWeighting extends WeightingScheme {
  def name: String = "cm"

  def getWeightedScore(prScore: Double, searchScore: Double): Double = {
    if (prScore == 0.0 || searchScore == 0.0) 0.0 else 1/(1/prScore + 1/searchScore)
  }
}
