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
  def pascal(c: Int, r: Int): Int = {
    // cal combinations(r, c)
    def factorial(n: Int): Int = {

      def factorial_help(cur: Int, n: Int): Int = {
        if (n == 0) cur else factorial_help(cur * n, n - 1)
      }

      factorial_help(1, n)

    }

    factorial(r) / (factorial(r - c) * factorial(c))

  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def help(cnt: Int, cc: List[Char]): Boolean = {
      if (cc.isEmpty)
        if (cnt == 0) true
        else false
      else if (cc.head == ')')
        if (cnt > 0) help(cnt-1, cc.tail)
        else false
      else if (cc.head == '(')
        help(cnt+1, cc.tail)
      else
        help(cnt, cc.tail)
    }

    help(0, chars)

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def dfs(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money > 0 && !coins.isEmpty)
        dfs(money - coins.head, coins) + dfs(money, coins.tail)
      else 0
    }

    dfs(money, coins.sorted)

  }

}
