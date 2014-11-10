object sheet1 {
	val f: (Int => Int) = {(i:Int) => if (i==20) 1 else i+1}
	def t: List[List[Int]] = List( List(1,5,3,11), List(2,4,6))

	Permutation.Identity.Inverse

	val p = new Permutation(Map(1->2, 2->4, 4->1))
	p.Inverse
	p.signature


	val m : Map[Int,Int] = Map(6->6).withDefault(identity[Int])
	m.get(8)
	m.get(6)
	def myeval(g:Int=>Int): Int = {f(5)}
	myeval(f)
	myeval(m)
	m.asInstanceOf[Int=>Int]
}