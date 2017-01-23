import scala.collection.mutable.ListMap

/**
  * Created by ashish on 1/20/17.
  */

/*
* This class is the real worker
* This class has all the methods that
* prints/return the required result as per
* the assignment
* */

class MarksProcessor {

  case class Subject(subectName : String, subjectId : Int) {}

  private val math: Subject = Subject("Mathematics", 9028)
  private val science: Subject = Subject("Science", 2842)
  private val history: Subject = Subject("History", 2730)
  private val hindi: Subject = Subject("Hindi", 1003)
  private val english: Subject = Subject("English", 9110)

  private val subjects:List[Subject] = List(math, science, history, hindi, english)

  private val students:List[Student] = List(
    Student(902873, "Kunal"),
    Student(349394, "Shubhra"),
    Student(936239, "Ashish"),
    Student(293520, "Neha"),
    Student(882930, "Shubham"),
    Student(983402, "Shivangi"),
    Student(394201, "Prashant"),
    Student(384945, "Charmy"),
    Student(539204, "Akhil"),
    Student(904384, "Pankhuri")
  )

  private val marks : List[Marks] = List (

    Marks(math.subjectId, 293520, 89),
    Marks(science.subjectId, 293520, 69),
    Marks(hindi.subjectId, 293520, 87),
    Marks(english.subjectId, 293520, 91),

    Marks(math.subjectId, 983402, 92),
    Marks(science.subjectId, 983402, 56),
    Marks(hindi.subjectId, 983402, 78),
    Marks(english.subjectId, 983402, 91),

    Marks(math.subjectId, 384945, 99),
    Marks(science.subjectId, 384945, 97),
    Marks(hindi.subjectId, 384945, 98),
    Marks(english.subjectId, 384945, 91),

    Marks(math.subjectId, 904384, 40),
    Marks(science.subjectId, 904384, 30),
    Marks(hindi.subjectId, 904384, 23),
    Marks(english.subjectId, 904384, 21),

    Marks(math.subjectId, 902873, 90),
    Marks(science.subjectId, 902873, 30),
    Marks(hindi.subjectId, 902873, 43),
    Marks(english.subjectId, 902873, 91),

    Marks(science.subjectId, 349394, 68),
    Marks(math.subjectId, 349394, 62),
    Marks(english.subjectId, 349394, 27),
    Marks(history.subjectId, 349394, 36),

    Marks(science.subjectId, 936239, 72),
    Marks(math.subjectId, 936239, 92),
    Marks(hindi.subjectId, 936239, 89),
    Marks(history.subjectId, 936239, 34),

    Marks(english.subjectId, 882930, 68),
    Marks(science.subjectId, 882930, 45),
    Marks(math.subjectId, 882930, 49),
    Marks(history.subjectId, 882930, 53),

    Marks(hindi.subjectId, 394201, 51),
    Marks(english.subjectId, 394201, 93),
    Marks(history.subjectId, 394201, 56),
    Marks(science.subjectId, 394201, 34),

    Marks(science.subjectId, 539204, 90),
    Marks(math.subjectId, 539204, 49),
    Marks(english.subjectId, 539204, 32),
    Marks(hindi.subjectId, 539204, 72)

  )

  /*
* percentage : pass/fail criteria
*
* status : pass/fail status
*
* subjectId : particular subject ID
*
* */
  def tellMePassFail(subId : Int, percentage : Float, status : String) : String = {

    status.toLowerCase() match {
      case "pass" => "Pass Count : "+marks.filter(_.subjectId == subId ).partition(_.marksObtained >= percentage)._1.size.toString
      case "fail" => "Fail Count : "+marks.filter(_.subjectId == subId ).partition(_.marksObtained < percentage)._1.size.toString
      case _ => "Give right pass/fail status"
    }

  }

  /*
  * Input:- (subjectId, count, top/bottom)
  * Output:- based on the last input(top/bottom), output the
  * students details who have scored max/min in that subjectId
  * */
  def getBestWorstStudents(subId: Int, count: Int, topBottom: String): Unit = {
    println(s"---- $topBottom $count students in ${subjects.filter(_.subjectId == subId)(0).subectName}----")
    topBottom.toLowerCase match {
      case "top" => {
        val top5 = marks.filter(_.subjectId == subId).sortBy(_.marksObtained).reverse.slice(0,count)
        top5.foreach( top=> {
          students.foreach( student => if(student.id == top.studentId) println(student.name+" "+top.marksObtained) )
        })
      }
      case "bottom" =>val top5 = marks.filter(_.subjectId == subId).sortBy(_.marksObtained).slice(0,count)
        top5.foreach( top=> {
          students.foreach( student => if(student.id == top.studentId) println(student.name+" "+top.marksObtained) )
        })
      case _ => println("Pass correct options in function parameter")
    }
  }

  /*
  * Input:-
  * (top/bottom, count)
  * OutPut:-
  * Overall top/least scorer based on all the subjects score, fetch students name
  * count- input defines that how much students name are to be printed on console
  *
  * */

  def topLeastScorer(topBottom : String, count : Int) : Any = {
    println(s"---- $count $topBottom scorers are ----")
    val studentsAggregate:ListMap[Long, Float] = getAggrigateMarks

    topBottom.toLowerCase() match {
      case "bottom" => {
            studentsAggregate.toList.sortBy(_._2).slice(0, count).foreach(aggregate => {
            println(students.filter(_.id == aggregate._1)(0).name + " : " + aggregate._2)
          }
        )
      }
      case "top" => {
            studentsAggregate.toList.sortBy(_._2).reverse.slice(0, count).foreach(aggregate => {
            println(students.filter(_.id == aggregate._1)(0).name + " : " + aggregate._2)
          }
        )
      }
      case _ => println("Choose a right option (top | bottom)")
    }
  }

  /*
  * Input:-
  * (percentage, good_scholarship, normal_or_no_scholarship)
  * Output:- two groups of students with the amount of scholarship
  *
  * */

  def scholarshipGroups(percentage : Float, goodScholarship : Int, normalScholarsip : Int): Unit = {

    val studentsAggregate:ListMap[Long, Float] = getAggrigateMarks

    /*
    * Problem statement is not clear
    * Need to talk to Anjum Sir
    * */
  }

  /*
  * Input:-
  * (pass/fail, percentage)
  * count and print the number of students and all names who are passed/fail,
  * Pass or fail would be decided by percentage input field.
  * */

  def passFail(passFail : String, percentage : Float): Unit = {

    println(s"---- $passFail students as per ${percentage}% criteria ----")


    val studentsAggregate:ListMap[Long, Float] = getAggrigateMarks


    passFail.toLowerCase match {
      case "fail" => {
        studentsAggregate.toList.sortBy(_._2).foreach( aggregate => {
          if(aggregate._2 / 4 < percentage) {
            println(students.filter(_.id == aggregate._1)(0).name + " " + aggregate._2 / 4 )
          } else return
        })
      }
      case "pass" => {
        studentsAggregate.toList.sortBy(_._2).reverse.foreach( aggregate => {
          if(aggregate._2 / 4 > percentage) {
            println(students.filter(_.id == aggregate._1)(0).name + " " + aggregate._2 / 4 )
          } else return
        })
      }
      case _ => println("Enter a correct option (pass | fail)")
    }

  }

  /*Find the student(s) who have scored 95% or above and print its details.*/

  def studentsWithPecentageGreaterOrEqualTo(precentage : Float): Unit = {

    println(s"---- Student who scored ${precentage}% or more ----")

    val studentsAggregate:ListMap[Long, Float] = getAggrigateMarks

    studentsAggregate.toList.sortBy(_._2).reverse.foreach( aggregate => {
      if(aggregate._2 / 4 > precentage) {
        println(students.filter(_.id == aggregate._1)(0).name + " " + aggregate._2 / 4 )
      } else return
    })

  }

  def printReportCard(): Unit = {

    println("---- Report card of students ----")

    var sumOfMarks : Float = 0

    students.foreach( student => {
      print(f"${student.name}%-10s \t")
      marks.groupBy( _.studentId == student.id)(true).foreach(x => {
        sumOfMarks += x.marksObtained
        print(x.marksObtained.toInt + " ")
      })
      println(sumOfMarks)
      sumOfMarks = 0
    })

  }



  private def getAggrigateMarks():ListMap[Long, Float] ={

    var sumOfMarks:Float = 0
    val studentsAggregate:ListMap[Long, Float] = ListMap()

    students.foreach( student => {
      marks.groupBy( _.studentId == student.id)(true).foreach(x => sumOfMarks += x.marksObtained)
      studentsAggregate(student.id) = sumOfMarks
      sumOfMarks = 0
    })

    studentsAggregate

  } /*Function ends here*/
}