package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import java.security.KeyStore.TrustedCertificateEntry

object AoCDay04:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type Assignment = (Range, Range)

  ////////////
  // Part 1 //
  ////////////

  def solve1(assignments: Seq[Assignment]): Int = assignments.count(fullyContains)

  def fullyContains(range1: Range, range2: Range): Boolean =
    (range1.contains(range2.start) && range1.contains(range2.end)) ||
      (range2.contains(range1.start) && range2.contains(range1.end))

  ////////////
  // Part 2 //
  ////////////

  def solve2(assignments: Seq[Assignment]): Int = assignments.count(overlaps)

  def overlaps(range1: Range, range2: Range): Boolean =
    range1.contains(range2.start) || range1.contains(range2.end) ||
      range2.contains(range1.start) || range2.contains(range1.end)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day04(): Unit =
    val templates = IO
      .readFileLines("y2022/day04.txt")
      .map(parseRanges)
    println(solve1(templates))
    println(solve2(templates))

  def parseRanges(line: String): Assignment =
    val rangesArray: Array[Range] = line
      .split(',')
      .map(_.split('-') match
        case Array(startStr, endStr) => startStr.toInt to endStr.toInt
      )
    rangesArray match {
      case Array(firstRange, secondRange) => (firstRange, secondRange)
    }
