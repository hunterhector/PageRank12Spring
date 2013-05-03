package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:54 PM
 */
/**
 * Parent class for weighting scheme
 * All method should extend this to define a new weighting scheme
 */
trait WeightingScheme {
    def name:String
    def getWeightedScore(prScore:Double,searchScore:Double):Double
}
