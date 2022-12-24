package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import com.sonowz.aoc.common.Grid2D.{given, *}

import scala.util.chaining.given
import cats.implicits.toBifunctorOps

import scala.annotation.{tailrec, targetName}

object AoCDay17:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type Pos = (Int, Int)

  extension (p: Pos)
    @targetName("addPos")
    def +(other: Pos) = (p._1 + other._1, p._2 + other._2)

  enum Jet:
    case Left, Right

  case class Rock(shape: Set[Pos], offset: Pos)

  case class CaveMap(rocks: Set[Pos], leftWall: Int, rightWall: Int) {
    def getHeight: Int = rocks.map(_._2).minOption.getOrElse(0).abs
  }

  ////////////
  // Part 1 //
  ////////////

  def solve1(jetPattern: Seq[Jet]): Int =
    val initJetStream: Seq[Jet] = LazyList.continually(jetPattern).flatten
    val initCaveMap: CaveMap = CaveMap((-3 to 5).map((_, 0)).toSet, -3, 5)
    val finalCaveMap = generateRock
      .take(2022)
      .foldLeft((initCaveMap, initJetStream)) { (state, rock) =>
        val (caveMap, jetStream) = state
        val fallOffset = (0, -caveMap.getHeight - 4)
        fallRock(caveMap, jetStream, rock(fallOffset))
      }
      ._1
    finalCaveMap.getHeight

  // Generate infinite list
  def generateRock: LazyList[Pos => Rock] =
    val rocks = Seq(
      Set((0, 0), (1, 0), (2, 0), (3, 0)),
      Set((1, 0), (0, -1), (1, -1), (2, -1), (1, -2)),
      Set((0, 0), (1, 0), (2, 0), (2, -1), (2, -2)),
      Set((0, 0), (0, -1), (0, -2), (0, -3)),
      Set((0, 0), (1, 0), (1, -1), (0, -1))
    )
    LazyList.continually(rocks).flatten.map(shape => (offset: Pos) => Rock(shape, offset))

  @tailrec
  def fallRock(caveMap: CaveMap, jetStream: Seq[Jet], rock: Rock): (CaveMap, Seq[Jet]) =
    jetStream match
      case Seq() => throw RuntimeException("jetStream is empty!")
      case jet +: nextJetStream =>
        val offsetJet: Pos = jet match
          case Jet.Right => rock.offset.bimap(_ + 1, identity)
          case Jet.Left  => rock.offset.bimap(_ - 1, identity)
        val offsetJetted =
          if !overlaps(caveMap, rock.copy(offset = offsetJet))
          then offsetJet
          else rock.offset
        val offsetDown = offsetJetted.bimap(identity, _ + 1)
        if !overlaps(caveMap, Rock(rock.shape, offsetDown))
        then fallRock(caveMap, nextJetStream, rock.copy(offset = offsetDown))
        else {
          val newCaveMap = caveMap.copy(rocks = caveMap.rocks | rock.shape.map(_ + offsetJetted))
          (newCaveMap, nextJetStream)
        }

  def overlaps(caveMap: CaveMap, rock: Rock): Boolean =
    rock.shape.exists { pos =>
      val rockPos = pos + rock.offset
      val rockX = rockPos._1
      caveMap.rocks(rockPos) || rockX <= caveMap.leftWall || rockX >= caveMap.rightWall
    }

  ////////////
  // Part 2 //
  ////////////

  def solve2(jetPattern: Seq[Jet]): Long =
    type JetIndex = Int // Index of 'jetPattern' used

    val jetStream: IndexedSeq[Jet] = (jetPattern ++ jetPattern).toIndexedSeq
    val initCaveMap: CaveMap = CaveMap((-3 to 5).map((_, 0)).toSet, -3, 5)
    val states: LazyList[(CaveMap, JetIndex)] = generateRock
      .scanLeft((initCaveMap, 0)) { (state, rock) =>
        val (caveMap, jetIndex) = state
        val fallOffset = (0, -caveMap.getHeight - 4)
        val (newCaveMap, leftoverStream) = fallRock(caveMap, jetStream.drop(jetIndex), rock(fallOffset))
        val newJetIndex = (jetStream.length - leftoverStream.length) % jetPattern.length
        (newCaveMap, newJetIndex)
      }

    // Slice y-axis of caveMap with 'CAVE_WINDOW_SIZE', and find cycle with them
    val CAVE_WINDOW_SIZE = 1000
    val windowedStates: LazyList[(Set[Pos], JetIndex)] = states.map { state =>
      val (caveMap, jetIndex) = state
      val minY = -caveMap.getHeight
      val caveWindow = caveMap.rocks.filter(_._2 < minY + CAVE_WINDOW_SIZE).map(_.bimap(identity, _ - minY))
      (caveWindow, jetIndex)
    }
    val cycle: Cycle = findCycle(windowedStates)

    def getStateHeight(i: Int): Int = states(i)._1.getHeight
    val startHeight: Long = getStateHeight(cycle.start.toInt)
    val cycleHeight: Long = getStateHeight(cycle.start.toInt + cycle.period.toInt) - startHeight
    val cyclingCount = 1_000_000_000_000L - cycle.start
    val quot = cyclingCount / cycle.period
    val rem = cyclingCount % cycle.period
    val extraHeight = getStateHeight(cycle.start.toInt + rem.toInt) - startHeight
    startHeight + quot * cycleHeight + extraHeight

  case class Cycle(start: Long, period: Long)

  def findCycle[T](states: LazyList[T]): Cycle =
    var history: Vector[T] = Vector()
    var ret: Cycle = null
    states.find { state =>
      val i = history.lastIndexOf(state)
      if i > 0
      then {
        ret = Cycle(i, history.size - i)
        true
      } else {
        history = history :+ state
        false
      }
    }
    assert(ret != null)
    ret

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day17(): Unit =
    val jetPattern = IO
      .readFile("y2022/day17.txt")
      .pipe(parseJetPattern)
    println(solve1(jetPattern))
    println(solve2(jetPattern))

  def parseJetPattern(line: String): Seq[Jet] =
    line.map {
      case '<' => Jet.Left
      case '>' => Jet.Right
    }
