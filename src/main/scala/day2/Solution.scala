package day2

object Solution extends App {

  val ADD = 1
  val MULTIPLY = 2
  val EXIT = 99

  def evaluate(memory: Vector[Int], pc: Int = 0): Vector[Int] = {
    memory(pc) match {
      case EXIT => memory
      case ADD =>
        val res = memory(memory(pc + 1)) + memory(memory(pc + 2))
        evaluate(memory.updated(memory(pc + 3), res), pc + 4)
      case MULTIPLY =>
        val res = memory(memory(pc + 1)) * memory(memory(pc + 2))
        evaluate(memory.updated(memory(pc + 3), res), pc + 4)
    }
  }

  val testInput = Vector(1,9,10,3,2,3,11,0,99,30,40,50)
  println(evaluate(testInput))
  val testInput2: Vector[Int] = Vector(1, 1, 1, 4, 99, 5, 6, 0, 99)
  println(evaluate(testInput2))

  val puzzleInput: Vector[Int] = Vector(1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 19, 5, 23, 2, 9, 23, 27, 1, 5, 27, 31, 1, 5, 31, 35, 1, 35, 13, 39, 1, 39, 9, 43, 1, 5, 43, 47, 1, 47, 6, 51, 1, 51, 13, 55, 1, 55, 9, 59, 1, 59, 13, 63, 2, 63, 13, 67, 1, 67, 10, 71, 1, 71, 6, 75, 2, 10, 75, 79, 2, 10, 79, 83, 1, 5, 83, 87, 2, 6, 87, 91, 1, 91, 6, 95, 1, 95, 13, 99, 2, 99, 13, 103, 1, 103, 9, 107, 1, 10, 107, 111, 2, 111, 13, 115, 1, 10, 115, 119, 1, 10, 119, 123, 2, 13, 123, 127, 2, 6, 127, 131, 1, 13, 131, 135, 1, 135, 2, 139, 1, 139, 6, 0, 99, 2, 0, 14, 0)

  println(evaluate(puzzleInput.updated(1, 12).updated(2, 2)))

  val res = for {
    noun <- 1 to 99
    verb <- 1 to 99
    if evaluate(puzzleInput.updated(1, noun).updated(2, verb))(0) == 19690720
  } yield (noun, verb)

  println(res.map {case (noun: Int, verb: Int) => 100 * noun + verb })

}
