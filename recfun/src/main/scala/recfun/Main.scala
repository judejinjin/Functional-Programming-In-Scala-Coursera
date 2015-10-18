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
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    
    if( c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
    
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
     var counter = 0
     var flag = true
     
     for (i <- chars){ 
        if(i == '(')
          counter = counter + 1
          
        if(i == ')')
          counter = counter - 1
        
        if(counter < 0)
          flag = false
     }
     
     if(counter == 0 && flag == true)
       true
     else
       false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
   
    if(money == 0)
       1
    else if(money > 0 && !coins.isEmpty)
       countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else
       0
  }
}
