package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.collection.immutable.SortedMap
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.chaining.scalaUtilChainingOps

object AoCDay05:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Instruction(quantity: Int, from: Int, to: Int)
  type CrateState = SortedMap[Int, List[Char]]

  ////////////
  // Part 1 //
  ////////////

  def solve1(crateState: CrateState, instructions: List[Instruction]): String =
    instructions
      .foldLeft(crateState)(step)
      .values
      .map(_.head)
      .mkString

  def step(state: CrateState, ins: Instruction): CrateState =
    val fromStack = state(ins.from)
    val toStack = state(ins.to)
    val (loaded, newFromStack) = fromStack.splitAt(ins.quantity)
    val newToStack = loaded.foldLeft(toStack) { case (s, c) => c +: s }
    state.updated(ins.from, newFromStack).updated(ins.to, newToStack)

  ////////////
  // Part 2 //
  ////////////

  def solve2(crateState: CrateState, instructions: List[Instruction]): String =
    instructions
      .foldLeft(crateState)(step2)
      .values
      .map(_.head)
      .mkString

  def step2(state: CrateState, ins: Instruction): CrateState =
    val fromStack = state(ins.from)
    val toStack = state(ins.to)
    val (loaded, newFromStack) = fromStack.splitAt(ins.quantity)
    val newToStack = loaded ::: toStack
    state.updated(ins.from, newFromStack).updated(ins.to, newToStack)

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day05(): Unit =
    val (crateState, instructions) = IO
      .readFile("y2022/day05.txt")
      .pipe(InputParser.apply)
    println(solve1(crateState, instructions))
    println(solve2(crateState, instructions))

  object InputParser extends StdLexical with RegexParsers:

    override def skipWhitespace: Boolean = false

    def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

    def apply(input: String): (CrateState, List[Instruction]) =
      parseAll(crateState ~ instructions, input) match {
        case Success(crateState ~ instructions, _) => (crateState, instructions)
      }

    type Crate = Option[Char]

    def crateState: Parser[CrateState] =
      val crateLine = rep1sep(crate, " ")
      rep1sep(crateLine, "\n") ~ """\n 1[^\n]*\n\n""".r ^^ { case crates ~ _ =>
        val crateLength = crates.map(_.length).max
        val initState: CrateState = SortedMap((1 to crateLength).map(_ -> List[Char]())*)
        val foldFn = (line: Seq[Crate], state: CrateState) =>
          line.zipWithIndex
            .foldLeft(state) {
              case (s, (None, _))    => s
              case (s, (Some(c), i)) => s.updatedWith(i + 1)(_.map(c +: _))
            }
        crates.foldRight(initState)(foldFn)
      }

    def crate: Parser[Crate] =
      val someCrate = "[" ~> letter <~ "]" ^^ Some
      val emptyCrate = "   " ^^^ None
      someCrate | emptyCrate

    def instructions: Parser[List[Instruction]] =
      rep1sep(instruction, "\n")

    def instruction: Parser[Instruction] =
      "move " ~ integer ~ " from " ~ integer ~ " to " ~ integer ^^ {
        _ match
          case _ ~ quantity ~ _ ~ from ~ _ ~ to => Instruction(quantity, from, to)
      }
