package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.given

object AoCDay09:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  enum Motion:
    case Up(steps: Int)
    case Down(steps: Int)
    case Left(steps: Int)
    case Right(steps: Int)

  type Pos = (Int, Int)

  case class PhysicsGrid(tailVisited: Set[Pos], knots: List[Pos])

  object PhysicsGrid:
    def apply(knotCount: Int): PhysicsGrid = PhysicsGrid(Set(), List.fill(knotCount + 1)((0, 0)))

  ////////////
  // Part 1 //
  ////////////

  def solve1(motions: Seq[Motion]): Int =
    motions.foldLeft(PhysicsGrid(1))(simulateMotion).tailVisited.size

  def simulateMotion(grid: PhysicsGrid, motion: Motion): PhysicsGrid =
    var newKnots = List[Pos]() // Write positions of new knots as side effect

    val head :: knots = grid.knots
    val headMotions: Seq[Pos] = getHeadMotion(head, motion)
    val tailMotions: Seq[Pos] =
      knots.foldLeft(headMotions) { (prevKnotMotions, knot) =>
        newKnots :+= prevKnotMotions.last
        prevKnotMotions.scanLeft(knot)(updateKnotPos).tail
      }
    newKnots :+= tailMotions.last
    val newTailVisited = tailMotions.foldLeft(grid.tailVisited)(_ + _)
    PhysicsGrid(newTailVisited, newKnots)

  def getHeadMotion(pos: Pos, motion: Motion): Seq[Pos] =
    import Motion.*
    motion match
      case Up(steps)    => (1 to steps).scanLeft(pos)((p, _) => (p._1, p._2 + 1)).tail
      case Down(steps)  => (1 to steps).scanLeft(pos)((p, _) => (p._1, p._2 - 1)).tail
      case Left(steps)  => (1 to steps).scanLeft(pos)((p, _) => (p._1 - 1, p._2)).tail
      case Right(steps) => (1 to steps).scanLeft(pos)((p, _) => (p._1 + 1, p._2)).tail

  def updateKnotPos(tailPos: Pos, headPos: Pos): Pos =
    val (tx, ty) = tailPos
    val (hx, hy) = headPos
    val isDirectlyDifferent = tx != hx ^ ty != hy
    if isDirectlyDifferent
    then
      if math.abs(hx - tx) + math.abs(hy - ty) <= 1 then tailPos
      else if ty < hy then (tx, ty + 1)
      else if hy < ty then (tx, ty - 1)
      else if tx < hx then (tx + 1, ty)
      else (tx - 1, ty)
    else (
      if math.abs(hx - tx) + math.abs(hy - ty) <= 2 then tailPos
      else if tx < hx && ty < hy then (tx + 1, ty + 1)
      else if hx < tx && ty < hy then (tx - 1, ty + 1)
      else if hx < tx && hy < ty then (tx - 1, ty - 1)
      else (tx + 1, ty - 1)
    )

  ////////////
  // Part 2 //
  ////////////

  def solve2(motions: Seq[Motion]): Int =
    motions.foldLeft(PhysicsGrid(9))(simulateMotion).tailVisited.size

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day09(): Unit =
    val motions = IO
      .readFileLines("y2022/day09.txt")
      .map(parseMotion)
    println(solve1(motions))
    println(solve2(motions))

  def parseMotion(line: String): Motion =
    line.split(" ") match
      case Array("U", x) => Motion.Up(x.toInt)
      case Array("D", x) => Motion.Down(x.toInt)
      case Array("L", x) => Motion.Left(x.toInt)
      case Array("R", x) => Motion.Right(x.toInt)
