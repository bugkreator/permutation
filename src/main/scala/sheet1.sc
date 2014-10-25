object sheet1 {
	val f: (Int => Int) = {(i:Int) => if (i==20) 1 else i+1}
	def t: List[Int] = List(1,5,3,11,1)

	val p = new Permutation(t)

	Permutation.allIntegers.map(p)


}