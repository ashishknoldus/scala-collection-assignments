/**
  * Created by ashish on 1/20/17.
  *
  *
  * This is assignment 1
  *
  *
  */
class Main {

}

object Main {

  /*
  *
  *main() has been kept separate to keep code clean
  *
  */

  def main(args: Array[String]) {

    val marksProcessor = new MarksProcessor

    println(marksProcessor.tellMePassFail(1003, 50, "fail"))

    marksProcessor.getBestWorstStudents(1003, 3, "bottom")

    marksProcessor.topLeastScorer("bottom", 2)

    marksProcessor.passFail("pass", 50)

    marksProcessor.studentsWithPecentageGreaterOrEqualTo(70)

    marksProcessor.printReportCard()
  }
}
