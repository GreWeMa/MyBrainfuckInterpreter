import scala.io.StdIn

object MyBrainfuckInterpreterInScala {

  def main(args: Array[String]): Unit = {
    val program = StdIn.readLine().toCharArray
    val executionPointer = new Pointer()
    val array = new Array[Int](30000)
    val pointer = new Pointer()
    var steps = 0
    while (executionPointer.value < program.length) {
      execute(program, executionPointer, array, pointer)
      steps += 1
    }
    println()
    println(s"steps taken = $steps")
  }

  def execute(program: Array[Char], executionPointer: Pointer, array: Array[Int], pointer: Pointer): Unit =
    program(executionPointer.value) match {
      case '>' => pointer.increment(); executionPointer.increment()
      case '<' => pointer.decrement(); executionPointer.increment()
      case '+' => array(pointer.value) += 1; executionPointer.increment()
      case '-' => array(pointer.value) -= 1; executionPointer.increment()
      case '.' => print(array(pointer.value).toChar); executionPointer.increment()
      case ',' => array(pointer.value) = StdIn.readChar(); executionPointer.increment()
      case '[' =>
        if (array(pointer.value) == 0) executionPointer.set(findNext(program, executionPointer.get) + 1)
        else executionPointer.increment()
      case ']' =>
        if (array(pointer.value) != 0) executionPointer.set(findPrevious(program, executionPointer.get) + 1)
        else executionPointer.increment()
    }

  def findNext(program: Array[Char], position: Int): Int = {
    var braces = 0
    for (n <- (position+1) until program.length) program(n) match {
      case '[' => braces += 1
      case ']' =>
        if (braces == 0) return n
        else braces -= 1
      case _ =>
    }
    throw new RuntimeException("Bad program")
  }

  def findPrevious(program: Array[Char], position: Int): Int = {
    var braces = 0
    for (n <- (position-1) to (0, -1)) program(n) match {
      case ']' => braces += 1
      case '[' =>
        if (braces == 0) return n
        else braces -= 1
      case _ =>
    }
    throw new RuntimeException("Bad program")
  }
}

class Pointer(var value: Int = 0) {

  def increment() = value += 1

  def decrement() = value -= 1

  def set(value: Int) = this.value = value

  def get = value
}