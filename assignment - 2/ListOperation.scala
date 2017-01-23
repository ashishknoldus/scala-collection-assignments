class ListManipulation
{
    def sumOfList(list: List[Int]): Int = {
        if(list.isEmpty) 0 else list.head + sumOfList(list.tail) }
        
        def multipleOfElementsOfList(element: Int): Unit = {
            println(s"Table of $element:")
            for(multiplier<- 1 to 10)
            {
                print(element * multiplier+" " )
            }
            println("")
        }
    }

object ListOperation {
    def main(args: Array[String]) {
        val myobj=new ListManipulation
        val list=List(2,3,4,5,6,7)
        println("sum of elements of list "+myobj.sumOfList(list))
        list.map(s=> myobj.multipleOfElementsOfList(s))
        
    }
}
