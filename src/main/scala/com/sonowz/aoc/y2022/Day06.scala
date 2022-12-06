package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

object AoCDay06:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type Packet = String

  ////////////
  // Part 1 //
  ////////////

  def solve1(packet: Packet): Int = getFirstUniqueIndex(packet, 4)

  def getFirstUniqueIndex(packet: Packet, windowSize: Int): Int =
    (windowSize to packet.size)
      .find { i => packet.substring(i - windowSize, i).toSet.size == windowSize }
      .getOrElse(throw IllegalArgumentException("No such index!"))

  ////////////
  // Part 2 //
  ////////////

  def solve2(packet: Packet): Int = getFirstUniqueIndex(packet, 14)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day06(): Unit =
    val packet = IO.readFile("y2022/day06.txt")
    println(solve1(packet))
    println(solve2(packet))
