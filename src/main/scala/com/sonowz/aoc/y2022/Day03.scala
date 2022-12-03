package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO
import cats.implicits.toBifunctorOps

object AoCDay03:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Rucksack(first: List[Char], second: List[Char])
  case class ElfGroup(first: Rucksack, second: Rucksack, third: Rucksack)

  ////////////
  // Part 1 //
  ////////////

  def solve1(rucksacks: Seq[Rucksack]): Int =
    rucksacks
      .map(findDuplicate)
      .map(getPriority)
      .sum

  def findDuplicate(rucksack: Rucksack): Char =
    val intersection = rucksack.first.toSet.intersect(rucksack.second.toSet)
    assert(intersection.size == 1, "Duplicate count is not one!")
    intersection.head

  def getPriority(c: Char): Int =
    if 'a' <= c && c <= 'z' then c - 'a' + 1
    else if 'A' <= c && c <= 'Z' then c - 'A' + 27
    else throw IllegalArgumentException("Invalid item character!")

  ////////////
  // Part 2 //
  ////////////

  def solve2(rucksacks: Seq[Rucksack]): Int =
    val groups: Iterator[ElfGroup] = rucksacks.grouped(3).map {
      _ match
        case Seq(x, y, z) => ElfGroup(x, y, z)
    }
    groups
      .map(findBadge)
      .map(getPriority)
      .sum

  def findBadge(group: ElfGroup): Char =
    val getItemSet = (rucksack: Rucksack) => (rucksack.first ++ rucksack.second).toSet
    val firstSet = getItemSet(group.first)
    val secondSet = getItemSet(group.second)
    val thirdSet = getItemSet(group.third)
    val intersection = firstSet.intersect(secondSet).intersect(thirdSet)
    assert(intersection.size == 1, "Badge count is not one!")
    intersection.head

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day03(): Unit =
    val rucksacks = IO
      .readFileLines("y2022/day03.txt")
      .map(parseRucksack)
    println(solve1(rucksacks))
    println(solve2(rucksacks))

  def parseRucksack(line: String): Rucksack =
    val mid = line.length / 2
    val items = line
      .splitAt(mid)
      .bimap(_.toList, _.toList)
    Rucksack(items._1, items._2)
