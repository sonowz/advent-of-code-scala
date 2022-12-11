package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import java.lang.Package
import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.chaining.given
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical

object AoCDay11:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Monkey(
      id: Int,
      startItems: Seq[WorryLevel],
      operation: WorryLevel => WorryLevel,
      divTest: WorryLevel,
      testTrueId: Int,
      testFalseId: Int
  )

  type WorryLevel = Long
  type ItemState = IndexedSeq[(Queue[WorryLevel], Int)]

  ////////////
  // Part 1 //
  ////////////

  def solve1(monkeys: Seq[Monkey]): Int =
    var state: ItemState = Vector(monkeys.map(m => (Queue(m.startItems: _*), 0)): _*)
    (1 to 20).foreach { _ => state = step(state, monkeys, _ / 3) }
    state.view
      .map(_._2)
      .sorted(using Ordering[Int].reverse)
      .take(2)
      .product

  def step(state: ItemState, monkeys: Seq[Monkey], reliefFunction: WorryLevel => WorryLevel): ItemState =
    monkeys
      .foldLeft(state) { (state, monkey) =>
        val (monkeyItems, inspectCount) = state(monkey.id)
        val (trueItems, falseItems) = monkeyItems
          .map(x => reliefFunction(monkey.operation(x)))
          .partition(x => x % monkey.divTest == 0)
        state.zipWithIndex
          .map {
            case ((_, _), monkey.id)                => (Queue(), inspectCount + monkeyItems.length)
            case ((items, cnt), monkey.testTrueId)  => (items ++ trueItems, cnt)
            case ((items, cnt), monkey.testFalseId) => (items ++ falseItems, cnt)
            case ((items, cnt), _)                  => (items, cnt)
          }
      }

  ////////////
  // Part 2 //
  ////////////

  def solve2(monkeys: Seq[Monkey]): Long =
    val monkeyLcm: Long = monkeys.map(_.divTest).reduce(lcm)

    var state: ItemState = Vector(monkeys.map(m => (Queue(m.startItems: _*), 0)): _*)
    (1 to 10000).foreach { _ => state = step(state, monkeys, _ % monkeyLcm) }
    state.view
      .map(_._2.toLong)
      .sorted(using Ordering[Long].reverse)
      .take(2)
      .product

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day11(): Unit =
    val monkeys = IO
      .readFile("y2022/day11.txt")
      .pipe(MonkeysParser.apply)
    println(solve1(monkeys))
    println(solve2(monkeys))

  object MonkeysParser extends StdLexical with RegexParsers:

    override def skipWhitespace: Boolean = true

    def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

    def long: Parser[Long] = """\d+""".r ^^ (_.toLong)

    def apply(input: String): Seq[Monkey] =
      parseAll(monkey.*, input) match {
        case Success(parsedMonkeys, _) => parsedMonkeys
      }

    def monkey: Parser[Monkey] =
      monkeyId ~ startItems ~ operation ~ test ~ "If true:" ~ throwMonkey ~ "If false:" ~ throwMonkey ^^ {
        case id ~ items ~ op ~ t ~ _ ~ ifTrue ~ _ ~ ifFalse =>
          Monkey(id, items, op, t, ifTrue, ifFalse)
      }

    def monkeyId: Parser[Int] = "Monkey" ~> integer <~ ":"

    def startItems: Parser[List[WorryLevel]] = "Starting items:" ~> repsep(long, ", ")

    def operation: Parser[WorryLevel => WorryLevel] =
      val term: Parser[String | WorryLevel] = "old" | long
      val operator: Parser[(WorryLevel, WorryLevel) => WorryLevel] = ("+" | "*") ^^ {
        case "+" => (x: WorryLevel, y: WorryLevel) => x + y
        case "*" => (x: WorryLevel, y: WorryLevel) => x * y
      }
      "Operation: new =" ~> term ~ operator ~ term ^^ {
        case "old" ~ op ~ (t2: WorryLevel) => (x: WorryLevel) => op(x, t2)
        case "old" ~ op ~ "old"            => (x: WorryLevel) => op(x, x)
        case _                             => throw IllegalArgumentException("Unexpected operation!")
      }

    def test: Parser[WorryLevel] = "Test: divisible by" ~> long

    def throwMonkey: Parser[Int] = "throw to monkey" ~> integer
