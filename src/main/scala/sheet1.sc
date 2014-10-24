object sheet1 {
	def f(i: Int):Int = if (i==20) 1 else i+1

	val p = FunctionPermutation(f)
	val q = p.Inverse
	val z = p*p
	Permutation.allIntegers.map(z)
}