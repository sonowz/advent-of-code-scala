package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

object AoCDayXX:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type InputType = Int
  type OutputType = Int

  ////////////
  // Part 1 //
  ////////////

  def solve1(inputs: Seq[InputType]): OutputType =
    ???

  ////////////
  // Part 2 //
  ////////////

  def solve2(inputs: Seq[InputType]): OutputType =
    ???

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def DayXX(): Unit =
    val templates = IO
      .readFileLines("y2022/dayXX.txt")
      .map(parseTemplate)
    println(solve1(templates))
    // println(solve2(templates))

  def parseTemplate(line: String): InputType =
    ???
