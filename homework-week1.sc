package week1

object homework {

  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc*n, n -1)
    loop(1, n)
  }                                               //> factorial: (n: Int)Int
  
  def pascal(c: Int, r: Int): Int = {
    factorial(r)/(factorial(c)*factorial(r-c))
  }                                               //> pascal: (c: Int, r: Int)Int
  pascal(5,10)                                    //> res0: Int = 252
  
  def pascal_triangle(row: Int): Int = {
  println(row)
  10
  }                                               //> pascal_triangle: (row: Int)Int
  pascal_triangle(3)                              //> 3
                                                  //| res1: Int = 10
  
  def balance(chars: List[Char]): Boolean = {
    def acc(count: Int, ch: Char): Int = {
      if (ch == '(') count+1 else
        if (ch == ')') count-1 else count
      }
    def loop(count: Int, chars: List[Char]): Boolean = {
      if (count < 0) false else
        if (chars.tail.isEmpty)
          if (acc(count, chars.head) == 0) true
            else false
          else loop(acc(count, chars.head), chars.tail)
    }
    loop(0, chars)
  }                                               //> balance: (chars: List[Char])Boolean
  balance("f(ads))(fasf".toList)                  //> res2: Boolean = false
  balance("(if (zero? x) max (/ 1 x))".toList)    //> res3: Boolean = true
  balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList)
                                                  //> res4: Boolean = true
  balance(":-)".toList)                           //> res5: Boolean = false
  balance("())(".toList)                          //> res6: Boolean = false
  
  // def countChange(money: Int, coins: List[Int]): Int
  
  
  
}
