package com.marimon.kata


object AutomataKata {

  val rule90: Map[String, Int] = Map(
    "111" -> 0, "110" -> 1,
    "101" -> 0, "100" -> 1,
    "011" -> 1, "010" -> 0,
    "001" -> 1, "000" -> 0)

  def evolve(rule: Map[String, Int],
             currentState: Array[Int],
             numIterations: Int): Array[Array[Int]] = {

    def nextState(currentState: Array[Int]): Array[Int] = {
      val work = Array(0) ++ currentState ++ Array(0)

      val finalAcc: (Int, Int, Array[Int]) = work.foldLeft((-2, -1, Array.empty[Int])) {
        (acc, current) =>
          acc match {
            case (-2, -1, arr) => (-1, current, arr)
            case (-1, x, arr) => (x, current, arr)
            case (x, y, arr) =>
              val key: String = s"$x$y$current"
              val output: Int = rule(key)

              (y, current, arr ++ Array(output))
          }
      }
      finalAcc._3
    }

    numIterations match {
      case 0 =>
        Array(currentState)
      case _ =>
        val next: Array[Int] = nextState(currentState)
        Array(currentState) ++ evolve(rule, next, numIterations - 1)
    }

  }


}
