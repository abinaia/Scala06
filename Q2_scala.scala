import scala.io.StdIn._

object Z2 {

  def validateInput(name: String, marks: Int, totalMarks: Int): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be between 0 and total possible marks"))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks must be positive"))
    } else {
      (true, None)
    }
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    var name = ""
    var marks = 0
    var totalMarks = 0
    var percentage = 0.0
    var grade = 'D'
    var validInput = false

    while (!validInput) {
      name = readLine("Enter student's name: ")
      println("Enter marks: ")
      marks = readInt()
      println("Enter total marks: ")
      totalMarks = readInt()

      val (isValid, errorMessage) = validateInput(name, marks, totalMarks)
      if (isValid) {
        validInput = true
        percentage = (marks.toDouble / totalMarks) * 100
        grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _            => 'D'
        }
      } else {
        println(errorMessage.getOrElse("Invalid input"))
        println("Please enter valid data.")
      }
    }

    (name, marks, totalMarks, percentage, grade)
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks: $marks")
    println(s"Total Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f%%")
    println(s"Grade: $grade")
  }

  def main(args: Array[String]): Unit = {
    println("Enter student information:")
    val studentRecord = getStudentInfo
    println("Student Record:")
    printStudentRecord(studentRecord)
  }
}
