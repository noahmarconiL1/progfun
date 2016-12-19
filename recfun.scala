package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) { 1 }
    else if (c < 0 || c > r) { 0 }
    else { pascal(c - 1, r - 1) + pascal(c, r - 1) }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def run(open: Int, cs: List[Char]): Boolean = {

      if (cs.isEmpty) { open == 0 }
      else {
        val ch = cs.head

        val openTally = {
          if (ch == '(') { open + 1 }
          else if (ch == ')') { open - 1 }
          else { open }
        }

        // Result.
        if (ch == ')' && open <= 0) { false }
        else { run(openTally, cs.tail) }

      }
    }

    run(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) { 1 }
    else if (coins.isEmpty || money < 0) { 0 }
    else { countChange(money - coins.head, coins) + countChange(money, coins.tail) }
  }
}
