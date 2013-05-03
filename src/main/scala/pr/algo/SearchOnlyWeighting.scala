package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector
 * Date: 5/3/13
 * Time: 12:20 AM
 * To change this template use File | Settings | File Templates.
 */

/**
 * A dummy weighting scheme that always return search score
 */
class SearchOnlyWeighting extends WeightingScheme{
  def name: String = "so"

  def getWeightedScore(prScore: Double, searchScore: Double): Double = searchScore
}
