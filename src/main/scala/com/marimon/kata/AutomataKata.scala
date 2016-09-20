package com.marimon.kata


object AutomataKata {

  val rule90: Map[String, Int] = Map(
    "111" -> 0, "110" -> 1,
    "101" -> 0, "100" -> 1,
    "011" -> 1, "010" -> 0,
    "001" -> 1, "000" -> 0)

  private def nextState(rule: Map[String, Int], currentState: Array[Int]): Array[Int] =
    (Array(0) ++ currentState ++ Array(0)).sliding(3).map(key => rule(key.mkString(""))).toArray

  def evolve(rule: Map[String, Int],
             currentState: Array[Int],
             numIterations: Int): Array[Array[Int]] =
    (1 to numIterations).scanLeft(currentState)((state, _) => nextState(rule, state)).toArray

}
