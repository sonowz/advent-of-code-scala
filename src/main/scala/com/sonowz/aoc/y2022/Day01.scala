package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.given

object AoCDay01:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Inventory(items: List[Int])

  ////////////
  // Part 1 //
  ////////////

  def solve1(inputs: Seq[Inventory]): Int =
    inputs
      .map(_.items.sum)
      .max

  ////////////
  // Part 2 //
  ////////////

  def solve2(inputs: Seq[Inventory]): Int =
    inputs
      .map(_.items.sum)
      .sorted(using Ordering[Int].reverse)
      .take(3)
      .sum

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day01(): Unit =
    val inventories = IO
      .readFileLines("y2022/day01.txt")
      .pipe(parseInventories)
    println(solve1(inventories))
    println(solve2(inventories))

  def parseInventories(lines: Seq[String]): Seq[Inventory] =
    lines.toList
      .pipe(splitWhen[String])(_ == "")
      .map(lines => Inventory(lines.map(_.toInt)))

  def splitWhen[T](l: List[T])(pred: T => Boolean): List[List[T]] =
    splitBy(l)(pred) match
      case (xs, Nil) => List(xs)
      case (xs, ys)  => xs :: splitWhen(ys)(pred)

  def splitBy[T](l: List[T])(pred: T => Boolean): (List[T], List[T]) =
    l match
      case Nil                => (List(), List())
      case x :: xs if pred(x) => (List(), xs)
      case x :: xs =>
        val (xss, rem) = splitBy(xs)(pred)
        (x :: xss, rem)
