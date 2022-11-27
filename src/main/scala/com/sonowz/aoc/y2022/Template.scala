package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import java.io.File

object Template:

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
  def main: Unit =
    val templates = IO
      .readFileLines("y2022/day1.txt")
      .map(parseTemplate)
    println(solve1(templates))
    // println(solve2(templates))

  def parseTemplate(line: String): InputType =
    ???
