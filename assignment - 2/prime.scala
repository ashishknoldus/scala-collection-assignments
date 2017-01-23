


class Prime
{
    def findPrime(num: Int,factors: Int): Boolean = {
        if(num==1) false
        else if(num==2 || num==3 || factors==num) true
        else if(num % factors == 0) false
        else 
        findPrime(num,factors+1)
    }
}
object prime {
    def main(args: Array[String]) {
        val myobj=new Prime
        val mylist=List(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)
        val primelist=mylist.map(s=> myobj.findPrime(s,2))
        println(primelist)
    }
}
