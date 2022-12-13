package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.given
import scala.math.Ordered.given

object AoCDay13:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  enum Packet:
    case List(inner: Seq[Packet])
    case Integer(value: Int)

  given Ordering[Packet] with
    override def compare(x: Packet, y: Packet): Int = (x, y) match
      case (Packet.Integer(n1), Packet.Integer(n2)) => n1.compare(n2)
      case (Packet.List(l1), Packet.List(l2)) =>
        l1.lazyZip(l2).map(compare).find(_ != 0).getOrElse(l1.length.compare(l2.length))
      case (i1 @ _: Packet.Integer, l2 @ _: Packet.List) => Packet.List(List(i1)).compare(l2)
      case (l1 @ _: Packet.List, i2 @ _: Packet.Integer) => l1.compare(Packet.List(List(i2)))

  ////////////
  // Part 1 //
  ////////////

  def solve1(packetPairs: Seq[(Packet, Packet)]): Int =
    packetPairs.zipWithIndex
      .filter((pair, i) => pair._1 < pair._2)
      .map((pair, i) => i + 1)
      .sum

  ////////////
  // Part 2 //
  ////////////

  def solve2(packetPairs: Seq[(Packet, Packet)]): Int =
    val dividerPacket = (x: Int) => Packet.List(List(Packet.List(List(Packet.Integer(x)))))
    val actualPacket: Seq[Packet] =
      dividerPacket(2) +: dividerPacket(6) +: packetPairs.map((p1, p2) => Seq(p1, p2)).flatten

    val orderedPacket = actualPacket.sorted.zipWithIndex.map((p, i) => (p, i + 1))
    orderedPacket.find(_._1 == dividerPacket(2)).get._2 *
      orderedPacket.find(_._1 == dividerPacket(6)).get._2

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day13(): Unit =
    val packets = IO
      .readFileLines("y2022/day13.txt")
      .filter(_.nonEmpty)
      .map(parsePacket)
      .grouped(2)
      .map { case Seq(l, r) => (l, r) }
      .toSeq
    println(solve1(packets))
    println(solve2(packets))

  def parsePacket(line: String): Packet = parsePacket_(line)._1.get

  def parsePacket_(str: String): (Option[Packet], String) =
    str.head match
      case ']' => (None, str.tail)
      case '[' =>
        var rem = ""
        val inner = List.unfold(str) {
          _.splitAt(1) match
            case ("]", tail) => rem = tail; None
            case (_, tail) =>
              parsePacket_(tail) match
                case (Some(p), rem_) => Some((p, rem_))
                case (None, rem_)    => rem = rem_; None
        }
        (Some(Packet.List(inner)), rem)
      case _ =>
        val integerStr = str.takeWhile(c => '0' <= c && c <= '9')
        (Some(Packet.Integer(integerStr.toInt)), str.drop(integerStr.length))
