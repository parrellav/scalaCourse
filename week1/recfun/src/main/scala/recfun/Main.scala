package recfun
import common._

object Main {
  def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
          for (col <- 0 to row)
            print(pascal(col, row) + " ")
          println()
        }
    println("Exercise 2")
    checkBalance("(just an) example")
    checkBalance("())(")
    checkBalance("()")
    checkBalance("(")
    checkBalance(")")
    checkBalance("()(())")

    println()

    def checkBalance(s: String) = {
      println(s + (if(balance(s.toList)) " is balanced" else " is not balanced"))
    }
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

    def isBalanced(chars: List[Char], accumulator: Int): Boolean = {
      if(chars.isEmpty) return accumulator == 0
      else if (chars.head == '(') {
        isBalanced(chars.tail, accumulator+1)
      }
      else if (chars.head == ')') {
        if(accumulator > 0) isBalanced(chars.tail, accumulator-1) else false
      }
      else {
        isBalanced(chars.tail, accumulator)
      }
    }

    isBalanced(chars, 0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
