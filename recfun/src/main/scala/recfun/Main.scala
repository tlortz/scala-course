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
      if (c==0 || r==0 || r==1 || c==r) 1
      else pascal(c-1,r-1) + pascal(c,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      val parens = chars.filter(x => (x=='(' || x==')'))
      def loop(openPar:Int,balanced:Boolean, remnant:List[Char]):Boolean = {
          remnant match {
            case Nil => {
              if(openPar==0 && balanced) true
              else false
            }
            case head::Nil => remnant.head match {
              case '(' => loop(openPar+1,false,remnant.tail)
              case ')' => {
                if ((openPar-1) == 0) {
                  loop(0, balanced,remnant.tail)
                }
                else {loop(openPar-1,false,remnant.tail)}
              }
              case _ => loop(openPar,false,remnant.tail)
            }
            case head::tail => remnant.head match {
              case '(' => loop(openPar+1,balanced,remnant.tail)
              case ')' => {
                if ((openPar-1) >= 0) {
                  loop(openPar-1, balanced,remnant.tail)
                }
                else {loop(openPar-1,false,remnant.tail)}
              }
              case _ => loop(openPar,false,remnant.tail)
            }

        }
      }
      loop(0,true,parens)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =  {
      /*
      def inner(m:Int,c:List[Int],ctr:List[Int],res:List[List[Int]]): List[List[Int]] = {
        for (coin <- c) yield {
          if ((m-coin)==0) (coin::ctr)::res
          else if (m>coin) inner(m-coin,c,coin::ctr)
        }
      }

      def inner(m:Int,c:List[Int],acc:Int):Int = c match {
        case Nil => acc
        case head::tail => if ((m-c.head)==0) inner(m,c.tail,acc+1) else inner(m - c.head,c,acc)
      }
      */
      def inner(amount: Int, allCoins:List[Int], remCoins: List[Int],sols:Int):Int = {
        if(remCoins.isEmpty) {sols}
        else if ((amount-remCoins.head) >= 0) {
          if ((amount-remCoins.head)==0) {
            inner(amount,allCoins,remCoins.tail,sols+1)
          }
          else {
            inner(amount-remCoins.head,allCoins,allCoins,sols)
          }
        }
        else inner(amount,allCoins,remCoins.tail,sols)
      }
      /*
      def inner(master:List[Int],curr:List[Int],agg:List[List[Int]]): List[List[Int]] = {
        if (curr.isEmpty) agg
        else (if )
      }
      */

      if (money==0 || coins.isEmpty) 0
      else inner(money,coins,coins,0)//inner(money,coins,Nil,List(Nil)).groupBy(x => x).unzip._1.sum
    }

  }
