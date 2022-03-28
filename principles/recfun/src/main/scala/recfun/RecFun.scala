package recfun

import scala.collection.mutable.Stack

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    var stack = Stack[Char]()

    def balanceRec(message: List[Char]): Boolean = {
      if (message.length == 0) stack.length == 0
      else {
        message.head match {
          case '(' => stack.push(')')
          case ')' => {
            if (stack.length == 0) return false 
            else stack.pop()
          }
          case _ => ()
        }
        balanceRec(message.tail)
      }
    }

    balanceRec(chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.length == 0) 0 
    else if (money == 0) 1
    else if (coins.head > money) countChange(money, coins.tail)
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
