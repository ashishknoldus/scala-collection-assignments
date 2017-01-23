/*  aggregate the contents of two lists of same size into a single list
	List(1,2) and List("a", "b") results List(List(1, "a"), List(2, "b")) */

object Aggregate extends App {
	val list1=List(1,2)
	val list2=List("a","b")

	val list3=list1.zip(list2)
        val newlist=list3.map(x => (List(x. _1,x._2)))
	println(newlist)
}
