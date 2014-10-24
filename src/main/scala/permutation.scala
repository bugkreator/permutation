abstract class Permutation extends Function1[Int, Int]
{
	def Inverse: Permutation
	def * (that: Permutation) : Permutation
}

object Permutation {
	val MAX_INT: Int = 20
	def isValidInteger(i: Int) : Boolean = {i>=1 && i<=MAX_INT}
	val allIntegers = (1 to MAX_INT).toList
	val Identity : Permutation = FunctionPermutation((i:Int) =>i )
}

case class FunctionPermutation(f: (Int => Int)) extends Permutation
{
	private val fibers : Map[Int,List[Int]] = Permutation.allIntegers.groupBy(f)
	private val assertValid: Boolean = {
		// apply f to all integers. Make sure image lies inside allIntegers, and all fibers are singletons
		val isValid : Boolean =  fibers.toList.forall(p => Permutation.isValidInteger(p._1) && (p._2.length==1))
		if (isValid) true else (throw new Exception("Invalid Function Permutation"))
	}

	def apply(i :Int) : Int = {f(i)}
	def Inverse = FunctionPermutation(
		(i:Int) => fibers(i).head
	)

	// (this * that) (i) = this (that(i))
	def * (that: Permutation) : FunctionPermutation = FunctionPermutation (
		(i:Int) => this(that(i))
	)
}

/*
case class Involution(Int i, Int j) extends Permutation {
	def Inverse: Permutation = {this}

}*/