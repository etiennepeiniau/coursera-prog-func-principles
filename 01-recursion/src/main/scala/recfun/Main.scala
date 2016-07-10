package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c > r) 0
    else if (c == 0) 1
    else if (c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceInside(a: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty)
        if (a == 0) true
        else false
      else if (a < 0) false
      else {
        if (chars.head.equals('(')) balanceInside(a + 1, chars.tail)
        else if (chars.head.equals(')')) balanceInside(a - 1, chars.tail)
        else balanceInside(a, chars.tail)
      }
    balanceInside(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money > 0 && coins.nonEmpty)
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else 0
  }

}
