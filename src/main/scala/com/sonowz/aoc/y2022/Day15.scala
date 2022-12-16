package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.given

object AoCDay15:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  type Pos = (Int, Int)
  case class Sensor(pos: Pos)
  case class Beacon(pos: Pos)
  case class SensorReading(sensor: Sensor, closestBeacon: Beacon)

  ////////////
  // Part 1 //
  ////////////

  def solve1(sensorReadings: Seq[SensorReading]): Int =
    val targetY: Int = 2_000_000
    val possibleRanges: Seq[Range] = sensorReadings
      .map(reading => (reading.sensor, manhattanDist(reading.sensor.pos, reading.closestBeacon.pos)))
      .map((sensor, d) => getRangeInY(sensor.pos, d, targetY))
      .pipe(unionRange)
    val deviceInRangeCount: Int = sensorReadings
      .map(_.closestBeacon.pos)
      .distinct
      .count((x, y) => y == targetY && possibleRanges.exists(_.contains(x)))
    possibleRanges.map(_.length).sum - deviceInRangeCount

  def manhattanDist(p1: Pos, p2: Pos): Int = math.abs(p1._1 - p2._1) + math.abs(p1._2 - p2._2)

  def getRangeInY(center: Pos, range: Int, y: Int): Range =
    val xDiff = range - math.abs(center._2 - y)
    if xDiff < 0 then 0 until 0
    else (center._1 - xDiff) to (center._1 + xDiff)

  def unionRange(ranges: Seq[Range]): Seq[Range] =
    enum R:
      case Start(x: Int)
      case End(x: Int)
    val rangeFragments: Seq[R] = ranges
      .filter(_.nonEmpty)
      .flatMap(r => Seq(R.Start(r.start), R.End(r.end)))
      .sortBy {
        case R.Start(x) => x - 0.5
        case R.End(x)   => x.toDouble
      }

    var ret: Seq[Range] = Seq()
    var inclusiveCount: Int = 0
    var start: Option[Int] = None
    rangeFragments.foreach {
      case R.Start(s) =>
        if start.isEmpty then {
          start = Some(s)
        }
        inclusiveCount += 1
      case R.End(e) =>
        inclusiveCount -= 1
        if inclusiveCount == 0 then {
          ret :+= (start.get to e)
          start = None
        }
    }
    ret

  ////////////
  // Part 2 //
  ////////////

  def solve2(sensorReadings: Seq[SensorReading]): Long =
    val minV: Int = 0
    val maxV: Int = 4_000_000
    val sensorNonexistentRanges: Seq[(Sensor, Int)] = sensorReadings
      .map(reading => (reading.sensor, manhattanDist(reading.sensor.pos, reading.closestBeacon.pos)))

    var beaconX: Long = -1
    val beaconY: Long = (minV to maxV).find { y =>
      val discontinuousRange = sensorNonexistentRanges
        .map((sensor, d) => getRangeInY(sensor.pos, d, y))
        .pipe(unionRange)
        .find(range => minV < range.start && range.start <= maxV + 1)
      discontinuousRange match
        case None => false
        case Some(range) =>
          beaconX = range.start - 1
          true
    }.get
    beaconX * maxV + beaconY

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day15(): Unit =
    val sensorReadings = IO
      .readFileLines("y2022/day15.txt")
      .map(parseSensorReading)
    println(solve1(sensorReadings))
    println(solve2(sensorReadings))

  def parseSensorReading(line: String): SensorReading =
    val words = line.split(' ')
    val Array(sx, sy) = words.slice(2, 4).map(_.replaceAll("""[^-\d]""", "").toInt)
    val Array(bx, by) = words.slice(8, 10).map(_.replaceAll("""[^-\d]""", "").toInt)
    SensorReading(Sensor((sx, sy)), Beacon((bx, by)))
