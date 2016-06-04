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
    if (c == r || c < 1) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    val openingBracketsStack = new scala.collection.mutable.Stack[Char]

    def matchBreackets(str: List[Char]): Boolean = {
      if (str.isEmpty) true
      else if (str.head == '(') {
        openingBracketsStack.push(str.head)
        matchBreackets(str.tail)
      }
      else if (str.head == ')') {
        if (openingBracketsStack.nonEmpty) {
          val lastInStack = openingBracketsStack.pop()
          matchBreackets(str.tail)
        } else {
          false
        }
      } else {
        matchBreackets(str.tail)
      }
    }

    matchBreackets(chars)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
