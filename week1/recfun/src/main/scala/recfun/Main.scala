package recfun
import common._

object Main {
  def main(args: Array[String]) {
//    println("Pascal's Triangle")
//    for (row <- 0 to 10) {
//      for (col <- 0 to row)
//        print(pascal(col, row) + " ")
//      println()
//    }
    println("Exercise 2")
    val justAnExample = "(just an) example"
    print(justAnExample + " is " + balance(justAnExample.toList))
    println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def isEdge(c: Int, r: Int): Boolean = {
      return (c == 0 || r==c)
    }

    if(isEdge(c, r)) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isOpenParen(c: Char): Boolean = {
      return c == "(".getBytes()
    }

    def isCloseParen(c: Char): Boolean = {
      return c == ")".getBytes()
    }

    def countParens(chars: List[Char], accumulator: Int) {
      if(chars.isEmpty) {
        return true
      }
      else if (chars.head == "(".charAt(0)) {
        countParens(chars.tail, accumulator+1)
      }
      else if (chars.head == ")".charAt(0)) {
        countParens(chars.tail, accumulator-1)
      }
      countParens(chars.tail, accumulator)
    }
    return (countParens(chars, 0) == 0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
