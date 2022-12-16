package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import com.sonowz.aoc.common.Grid2D.{*, given}

import scala.annotation.tailrec
import scala.util.chaining.given

object AoCDay14:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type CaveMap = Map[Pos, Tile]
  type Pos = (Int, Int)

  enum Tile:
    case Air, Rock

    override def toString: String =
      this match
        case Air  => " "
        case Rock => "#"

  type RockPath = Seq[Pos]

  ////////////
  // Part 1 //
  ////////////

  def solve1(rockPaths: Seq[RockPath]): Int =
    val caveMap = drawCaveMap(rockPaths)
    List.unfold(caveMap)(sandfall(_).map((1, _))).sum

  def drawCaveMap(rockPaths: Seq[RockPath]): CaveMap =
    val allRockPos: Seq[Pos] = rockPaths.flatMap { rockPath =>
      rockPath
        .lazyZip(rockPath.tail)
        .flatMap(straightTo)
    }
    allRockPos
      .map((_, Tile.Rock))
      .toMap

  def straightTo(start: Pos, end: Pos): Seq[Pos] =
    if start._1 == end._1
    then (start._2 to (end._2, (end._2 - start._2).sign)).map((start._1, _))
    else (start._1 to (end._1, (end._1 - start._1).sign)).map((_, start._2))

  @tailrec
  def sandfall(caveMap: CaveMap, sandPos: Pos = (500, 0), abyssY: Int = 1000): Option[CaveMap] =
    val (x, y) = sandPos
    if y > abyssY then None
    else if caveMap.getOrElse((x, y + 1), Tile.Air) == Tile.Air then sandfall(caveMap, (x, y + 1), abyssY)
    else if caveMap.getOrElse((x - 1, y + 1), Tile.Air) == Tile.Air then sandfall(caveMap, (x - 1, y + 1), abyssY)
    else if caveMap.getOrElse((x + 1, y + 1), Tile.Air) == Tile.Air then sandfall(caveMap, (x + 1, y + 1), abyssY)
    else Some(caveMap.updated((x, y), Tile.Rock))

  ////////////
  // Part 2 //
  ////////////

  def solve2(rockPaths: Seq[RockPath]): Int =
    val caveMap = drawCaveMap(rockPaths)
    val floorY = caveMap.keys.maxBy(_._2)._2 + 2
    List
      .unfold(caveMap) { cave =>
        val newCave = sandfallWithFloor(cave, floorY)
        if newCave.get((500, 0)).contains(Tile.Rock) then None
        else Some((1, newCave))
      }
      .sum + 1

  @tailrec
  def sandfallWithFloor(caveMap: CaveMap, floorY: Int, sandPos: Pos = (500, 0)): CaveMap =
    val (x, y) = sandPos
    if y + 1 == floorY then caveMap.updated((x, y), Tile.Rock)
    else if caveMap.getOrElse((x, y + 1), Tile.Air) == Tile.Air then sandfallWithFloor(caveMap, floorY, (x, y + 1))
    else if caveMap.getOrElse((x - 1, y + 1), Tile.Air) == Tile.Air then
      sandfallWithFloor(caveMap, floorY, (x - 1, y + 1))
    else if caveMap.getOrElse((x + 1, y + 1), Tile.Air) == Tile.Air then
      sandfallWithFloor(caveMap, floorY, (x + 1, y + 1))
    else caveMap.updated((x, y), Tile.Rock)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day14(): Unit =
    val rockPaths = IO
      .readFileLines("y2022/Day14.txt")
      .map(parseRockPath)
    println(solve1(rockPaths))
    println(solve2(rockPaths))

  def parseRockPath(line: String): RockPath =
    line
      .split(" -> ")
      .map {
        _.split(',') match
          case Array(x, y) => (x.toInt, y.toInt)
      }
      .toSeq
