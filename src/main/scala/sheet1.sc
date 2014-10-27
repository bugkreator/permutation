object sheet1 {
	val f: (Int => Int) = {(i:Int) => if (i==20) 1 else i+1}
	def t: List[List[Int]] = List( List(1,5,3,11), List(2,4,6))

	Permutation.Identity.Inverse
	val p = new Permutation(t)
	p.Inverse
	p.signature

}