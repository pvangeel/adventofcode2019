package day4

object Solution extends App {


  def onlyIncreasingNumbers(s: String) = {
    s.toSeq.sliding(2).map { a => (a(0), a(1)) }.forall { case (a, b) => a <= b }
  }

  def hasRepeatingDigit(s: String) = {
    s.toSeq.sliding(2).map { a => (a(0), a(1)) }.exists { case (a, b) => a == b }
  }

  def hasExactly2RepeatingDigit(s: String) = {
    s.toSeq.groupBy( identity ).view.mapValues(_.size).toMap.values.exists( _ == 2)
  }

  val candidatesPart1 = for {
    password <- 197487 to 673251
    if onlyIncreasingNumbers(password.toString) && hasRepeatingDigit(password.toString)
  } yield password


  println(candidatesPart1.length)
  println(candidatesPart1.count(i => hasExactly2RepeatingDigit(i.toString)))

}
