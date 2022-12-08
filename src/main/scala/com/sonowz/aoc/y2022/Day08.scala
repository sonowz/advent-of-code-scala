package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import com.sonowz.aoc.common.Grid2D.{given, *}

import scala.collection.immutable.HashMap
import scala.util.chaining.given

object AoCDay08:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type Pos = (Int, Int)
  type TreeGrid = HashMap[Pos, Int]

  ////////////
  // Part 1 //
  ////////////

  def solve1(treeGrid: TreeGrid): Int = getVisiblePosList(treeGrid).length

  def getVisiblePosList(grid: TreeGrid): Seq[Pos] =
    val xRange = 0 until grid.size2D._1
    val yRange = 0 until grid.size2D._2
    val visibleFromLeft =
      yRange.flatMap(y => getIncrementingPosList(grid, xRange.map(x => (x, y)))).toSet
    val visibleFromRight =
      yRange.flatMap(y => getIncrementingPosList(grid, xRange.reverse.map(x => (x, y)))).toSet
    val visibleFromUp =
      xRange.flatMap(x => getIncrementingPosList(grid, yRange.reverse.map(y => (x, y)))).toSet
    val visibleFromDown =
      xRange.flatMap(x => getIncrementingPosList(grid, yRange.map(y => (x, y)))).toSet
    (visibleFromLeft | visibleFromRight | visibleFromUp | visibleFromDown).toSeq

  def getIncrementingPosList(grid: TreeGrid, posList: Seq[Pos]): Seq[Pos] =
    val initState = (Seq(), -1)
    def foldFn(state: (Seq[Pos], Int), p: Pos) =
      val (acc, lastHeight) = state
      val height = grid.get2D(p)
      val newHeight = math.max(height, lastHeight)

      if height > lastHeight
      then (p +: acc, newHeight)
      else (acc, newHeight)

    posList.foldLeft(initState)(foldFn)._1

  ////////////
  // Part 2 //
  ////////////

  def solve2(treeGrid: TreeGrid): Int =
    treeGrid.keys
      .map(p => calcScenicScore(treeGrid, p))
      .max

  def calcScenicScore(grid: TreeGrid, pos: Pos): Int =
    val deltas = Seq((0, 1), (0, -1), (1, 0), (-1, 0))
    deltas
      .map((dx, dy) => getViewingDistance(grid, pos, dx, dy))
      .product

  def getViewingDistance(grid: TreeGrid, pos: Pos, dx: Int, dy: Int): Int =
    val originHeight = grid.get2D(pos)
    def go(p: Pos): Int =
      val newP = (p._1 + dx, p._2 + dy)
      grid.get2DOption(newP) match
        case None                                   => 0
        case Some(height) if height >= originHeight => 1
        case Some(_)                                => 1 + go(newP)
    go(pos)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day08(): Unit =
    val treeGrid = IO
      .readFileLines("y2022/day08.txt")
      .pipe(parseTreeGrid)
    println(solve1(treeGrid))
    println(solve2(treeGrid))

  def parseTreeGrid(lines: Seq[String]): TreeGrid =
    lines.zipWithIndex
      .flatMap { case (line, y) =>
        line.toArray.zipWithIndex
          .map { case (c, x) => (x, y) -> c.toInt }
      }
      .pipe(xs => HashMap(xs: _*))
