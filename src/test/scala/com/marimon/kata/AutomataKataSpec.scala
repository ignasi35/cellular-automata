package com.marimon.kata

import org.scalatest.Matchers
import org.scalatest._

final class AutomataKataSpec extends FlatSpec with Matchers {

  behavior of "AutomataKata"

  val rule90 = AutomataKata.rule90

  it should "return the initial state when no iterations invoked" in {
    //arrange
    val initialState = Array(1, 1, 0, 1, 0, 1, 0, 1)
    val numIterations = 0
    // act
    val actual =
    AutomataKata.evolve(rule90, initialState, numIterations)
    // assert
    actual shouldBe Array(initialState)
  }
  it should "return numIteration+1 arrays given numIterations" in {
    val initialState = Array(1, 1, 0, 1, 0, 1, 0, 1)
    val numIterations = 4
    // act
    val evolutions = AutomataKata.evolve(rule90, initialState, numIterations)
    // assert
    evolutions.length shouldBe 5
  }
  it should "evolve a single state using rule-90" in {
    val initialState = Array(1, 1, 0, 1)
    val numIterations = 1
    val evolutions = AutomataKata.evolve(rule90, initialState, numIterations)
    evolutions shouldBe Array(initialState, Array(1, 1, 0, 0))
  }


}

object Main extends App {

  val initialState: Array[Int] =
    "00000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000".toCharArray.map {
      case '0' => 0
      case '1' => 1
    }

  private val results: Array[Array[Int]] = AutomataKata.evolve(AutomataKata.rule90, initialState, 25)

  val toPrint: Array[String] = results.map {
    arr => arr.map {
      case 1 => 'X'
      case 0 => ' '
    }.mkString("")
  }

  toPrint.foreach(println)

}