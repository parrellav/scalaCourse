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
    val isBalanced = if(balance(justAnExample.toList)) " is balanced" else " is not balanced"
    print(justAnExample + isBalanced)
    println()
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def isEdge(c: Int, r: Int): Boolean = {
      c == 0 || r==c
    }

    if(isEdge(c, r)) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    def isOpenParen(c: Char): Boolean = {
      c == "(".charAt(0)
    }

    def isCloseParen(c: Char): Boolean = {
      c == ")".charAt(0)
    }

    def countParens(chars: List[Char], accumulator: Int): Int = {
      if(chars.isEmpty) {
        accumulator
      }
      else if (isOpenParen(chars.head)) {
        countParens(chars.tail, accumulator+1)
      }
      else if (isCloseParen(chars.head)) {
        countParens(chars.tail, accumulator-1)
      }
      else {
        countParens(chars.tail, accumulator)
      }
    }

    countParens(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
