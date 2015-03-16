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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    def line(indexC: Int, indexR: Int): Int = {
      if (indexR <= 1 || indexC == 0 || indexR == indexC) 1
      else line(indexC - 1, indexR - 1) + line(indexC, indexR - 1)
    }

    if (r < 0) throw new IllegalArgumentException("Row needs to be upper than zero")
    if (r < c) throw new IllegalArgumentException("Column cannot be upper than Row")

    if (r == 0 || c == 0 || c == r) 1
    else line(c, r)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
        def checkChar(isOpen: Boolean, openCount: Int, characters: List[Char]): Boolean = {
      if (isOpen && characters.isEmpty) false
      else if (!isOpen && characters.isEmpty) true
      else if (!isOpen && characters.head == ')') false
      else if (characters.head == '(') checkChar(true, openCount + 1, characters.tail)
      else if (openCount == 1 && characters.head == ')') checkChar(false, openCount - 1, characters.tail)
      else if (openCount > 1 && characters.head == ')') checkChar(true, openCount - 1, characters.tail)
      else checkChar(isOpen, openCount, characters.tail)
    }
  
    checkChar(false, 0, chars)
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      
      def count(m: Int, c: List[Int]) : Int = {
        if (c.isEmpty) 0
        else if (m - c.head == 0) 1
        else if (m - c.head < 0) 0
        else countChange(m - c.head, c) + countChange(m, c.tail)
      }
      
      count(money, coins.sorted)
  }

}
