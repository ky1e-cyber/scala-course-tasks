package recfun

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
  def pascal(c: Int, r: Int): Int =
    if c < 0 || c > r 
      then 0 
      else if r == 0 
        then 1 
        else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def f(acc: Int, lst: List[Char]): Boolean =
      if acc < 0 
        then false
      else if !lst.isEmpty then 
        lst.head match {
          case '(' => f(acc + 1, lst.tail)
          case ')' => f(acc - 1, lst.tail)
          case _   => f(acc, lst.tail)
        }
       
      else 
        acc == 0
      
    f(0, chars)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if money < 0 then 0 else {
      if money == 0 then 1 else {
        coins match {
        
          case hd :: tl => {
            var acc = 0
            var currentSum = 0
            while (currentSum <= money) {
              acc += countChange(money - currentSum, tl)
              currentSum += hd
            }
            acc
          }

          case _ => 0
        } 
      }
    }
    
    
