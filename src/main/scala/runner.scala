/**
 * Created by orotem on 10/27/2014.
 */
object runner extends App{
	override def main(args: Array[String]) {
		val p1: Permutation = new Permutation(1,2)
		val p2: Permutation = new Permutation(2,3)
		println(p1*p2)
	}
}
