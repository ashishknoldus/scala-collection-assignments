/* print the table of each element in the List */

class TableOfVal{

def cal(l:List[Int])=
{
	for(a<-l)
	{	
                println(s"Table of $a")
		for(num<-1 to 10)
		{
			print(a*num+" ")
		}
              println("")
	}
}
}
object Table extends App{
val list=List(1,2,3)
val obj=new TableOfVal
obj.cal(list)
}
