import scala.util.matching.Regex

/**
  * Created by ilyas on 2017-02-18.
  */
object Config {
    val maxBranches: Int = 10
    val branchingSeed: Double = 0.1
    val specChars: Regex = "[-~!@#$^%&*()_+={}\\\\[\\\\]|;:\\\"'`<,>.?/\\\\\\\\]".r
    val inc : Int = 10
}
