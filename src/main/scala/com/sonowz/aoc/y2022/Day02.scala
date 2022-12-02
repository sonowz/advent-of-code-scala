package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import com.sonowz.aoc.y2022.AoCDay02.RPS.Paper

object AoCDay02:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  enum ABC:
    case A, B, C

  enum XYZ:
    case X, Y, Z

  enum RPS extends Ordered[RPS]:
    case Rock, Paper, Scissors

    override def compare(that: RPS): Int =
      (this, that) match
        case (Rock, Paper)     => 1
        case (Paper, Scissors) => 1
        case (Scissors, Rock)  => 1
        case (Paper, Rock)     => -1
        case (Scissors, Paper) => -1
        case (Rock, Scissors)  => -1
        case _                 => 0

  case class Guide(opponent: ABC, me: XYZ)

  ////////////
  // Part 1 //
  ////////////

  def solve1(guides: Seq[Guide]): Int =
    guides
      .map(calcGuideScore)
      .sum

  def calcGuideScore(guide: Guide): Int =
    import ABC.*
    import XYZ.*
    import RPS.*
    val opponentRPS = guide.opponent match
      case A => Rock
      case B => Paper
      case C => Scissors
    val meRPS = guide.me match
      case X => Rock
      case Y => Paper
      case Z => Scissors
    calcScore(opponentRPS, meRPS)

  def calcScore(opponent: RPS, me: RPS): Int =
    import RPS.*
    val shapeScore = me match
      case Rock     => 1
      case Paper    => 2
      case Scissors => 3
    val outcomeScore = opponent compare me match
      case 1  => 6
      case 0  => 3
      case -1 => 0
    shapeScore + outcomeScore

  ////////////
  // Part 2 //
  ////////////

  def solve2(guides: Seq[Guide]): Int =
    guides
      .map(calcGuideScore2)
      .sum

  def calcGuideScore2(guide: Guide): Int =
    import ABC.*
    import XYZ.*
    import RPS.*
    val opponentRPS = guide.opponent match
      case A => Rock
      case B => Paper
      case C => Scissors
    val meRPS = (opponentRPS, guide.me) match
      case (Rock, X)     => Scissors
      case (Rock, Z)     => Paper
      case (Paper, X)    => Rock
      case (Paper, Z)    => Scissors
      case (Scissors, X) => Paper
      case (Scissors, Z) => Rock
      case (rps, Y)      => rps
    calcScore(opponentRPS, meRPS)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day02(): Unit =
    val guides = IO
      .readFileLines("y2022/day02.txt")
      .map(parseGuide)
    println(solve1(guides))
    println(solve2(guides))

  def parseGuide(line: String): Guide =
    val Array(opponentStr, meStr) = line.split(' ')
    val opponent = opponentStr match
      case "A" => ABC.A
      case "B" => ABC.B
      case "C" => ABC.C
    val me = meStr match
      case "X" => XYZ.X
      case "Y" => XYZ.Y
      case "Z" => XYZ.Z
    Guide(opponent, me)
