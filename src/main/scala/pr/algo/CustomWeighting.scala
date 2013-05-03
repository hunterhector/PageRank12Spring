package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/2/13
 * Time: 11:48 PM
 */

/**
 * The geograpic average method weighting
 * @param w
 */
class CustomWeighting(w:Double) extends WeightingScheme {
  def name: String = "cm"

  def getWeightedScore(prScore: Double, searchScore: Double): Double = {
      math.pow(prScore,w) * math.pow(math.exp(searchScore),1-w)
   }
}
