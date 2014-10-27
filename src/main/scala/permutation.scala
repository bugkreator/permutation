class Permutation(val func: (Int => Int))  extends Function1[Int, Int]
{
	// specifying a permutation as a map, assume non-specified values are fixed by the permutation
	def this(m: Map[Int,Int]) {
		this ( (i:Int) => if (m.isDefinedAt(i)) m(i) else i )
	}

	// transposition (a b)
	def this(a: Int, b: Int) {
		this ( Map(a->b, b->a) )
	}

	def this (Cycles: List[List[Int]]) {
		this({
			val isCyclesValid: Boolean = if (Permutation.isValidCycleList(Cycles)) true else throw new Exception("Invalid Cycles List")
			Permutation.listsToCyclesMap(Cycles)
		})
	}

	private val fibers : Map[Int,List[Int]] = Permutation.allIntegers.groupBy(func)

	private val assertValid: Boolean = {
		// apply f to all integers. Make sure image lies inside allIntegers, and all fibers are singletons
		val isValid : Boolean =  fibers.toList.forall({case (image, fiber) => Permutation.isValidInteger(image) && (fiber.length==1)})
		if (isValid) true else (throw new Exception("Invalid Permutation"))
	}

	def apply(i: Int) : Int = {func(i)}
	def Inverse: Permutation = new Permutation( (i:Int) => fibers(i).head )
	def * (that: Permutation) : Permutation = new Permutation ( (i:Int) => this(that(i)) )

	def toCycles: List[List[Int]] =
	{
		def helper(ValueSet: Set[Int], accCycles:List[List[Int]],  currCycleValues: List[Int]) : (Set[Int], List[List[Int]], List[Int]) =
		{
			if (ValueSet.isEmpty)
				if (currCycleValues.isEmpty) (ValueSet, accCycles, currCycleValues)
				else throw new Exception ("Something went wrong")
			else
			if (currCycleValues.isEmpty) helper(ValueSet.tail, accCycles, List(ValueSet.head))
			else
			{
				val nextValue: Int = this.func(currCycleValues.last)
				if (nextValue == currCycleValues.head) // reached full cycle. If it's not a 1-cycle, add it to the result & reset current cycle
					helper(ValueSet - nextValue, if (currCycleValues.length == 1) accCycles else currCycleValues :: accCycles, Nil)
				else
					helper(ValueSet - nextValue, accCycles, currCycleValues :+ nextValue)
			}
		}

		val (s,l, c) = helper(Permutation.allIntegers.toSet, Nil, Nil) // simplify this!!
		l
	}

}

object Permutation {
	val MAX_INT: Int = 20
	def isValidInteger(i: Int) : Boolean = {i>=1 && i<=MAX_INT}
	val allIntegers = (1 to MAX_INT).toList
	val Identity : Permutation = new Permutation( (i:Int) => i)

	private def hasNoDuplicates(l: List[Any]): Boolean = {
		l.distinct.length == l.length
	}

	private def isValidCycleList(Cycles:List[List[Int]]) : Boolean =
	{
		hasNoDuplicates(Cycles.flatten)
	}

	private def listToCycleMap(InitialMap: Map[Int,Int], l:List[Int]):Map[Int,Int] = {
		def helper(current:List[Int], acc: Map[Int,Int]):Map[Int,Int] = {
			current match {
				case Nil => throw new Exception ("Empty. Did not expect to get here.")
				case x::Nil => acc // done
				case x::y::rest => helper(y::rest, acc.updated(x,y))
			}
		}
		// wrap the list by adding the first element to the end
		helper(l:+l.head, InitialMap)
	}

	private def listsToCyclesMap(ls: List[List[Int]]) : Map[Int, Int] = {
		ls.foldLeft[Map[Int,Int]](Map())(listToCycleMap)
	}
}

