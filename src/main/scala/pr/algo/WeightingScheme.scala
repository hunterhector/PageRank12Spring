package pr.algo

/**
 * Created with IntelliJ IDEA.
 * User: Hector, Zhengzhong Liu
 * Date: 5/1/13
 * Time: 10:54 PM
 */
trait WeightingScheme {
    def getWeight(prScore:Double,searchScore:Double):Double
}
