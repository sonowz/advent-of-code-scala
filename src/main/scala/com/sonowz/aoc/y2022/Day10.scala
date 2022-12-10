package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.collection.immutable.Queue
import scala.util.chaining.given

object AoCDay10:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  enum Instruction(cycle: Cycle):
    case NoOp extends Instruction(1)
    case AddX(value: Int) extends Instruction(2)

    def cycleReq: Cycle = cycle

  type Program = List[Instruction]
  type Cycle = Int

  case class Register(x: Int)

  object Register:
    def apply(): Register = Register(1)

  trait Sidecar:
    def sideEffect(register: Register, cycle: Cycle): Unit

  given defaultSidecar: Sidecar with
    def sideEffect(register: Register, cycle: Cycle): Unit = {}

  ////////////
  // Part 1 //
  ////////////

  def solve1(program: Program): Int =
    given sidecar: SignalStrengthSidecar = SignalStrengthSidecar()
    runProgram(program)(using sidecar)
    sidecar.getSignalStrength.sum

  class SignalStrengthSidecar extends Sidecar:

    private var signalStrengths: List[Int] = List()

    def getSignalStrength: List[Int] = signalStrengths

    def sideEffect(register: Register, cycle: Cycle): Unit =
      if cycle == 20 || (cycle - 20) % 40 == 0
      then signalStrengths :+= cycle * register.x

  def runProgram(program: Program)(using sidecar: Sidecar): Register =
    val initState: (Register, Cycle) = (Register(), 1)
    program
      .foldLeft(initState) { (state, ins) =>
        val (reg, curCycle) = state
        (0 until ins.cycleReq).foreach(i => sidecar.sideEffect(reg, curCycle + i))
        (step(reg, ins), curCycle + ins.cycleReq)
      }
      ._1

  def step(reg: Register, ins: Instruction): Register =
    import Instruction.*
    ins match
      case NoOp        => reg
      case AddX(value) => reg.copy(x = reg.x + value)

  ////////////
  // Part 2 //
  ////////////

  def solve2(program: Program): String =
    given sidecar: CRTDrawSidecar = CRTDrawSidecar()
    runProgram(program)(using sidecar)
    sidecar.getCRTScreen

  class CRTDrawSidecar extends Sidecar:

    enum Pixel:
      case Lit, Dark

      override def toString: String =
        this match
          case Lit  => "██"
          case Dark => "  "

    private var crt: List[List[Pixel]] = List()
    private var currentCRTRow: Queue[Pixel] = Queue()

    def getCRTScreen: String = crt.map(_.mkString("")).mkString("\n")

    def sideEffect(register: Register, cycle: Cycle): Unit =
      val currentCRTCol = currentCRTRow.length
      val spritePos = register.x
      val newPixel =
        if math.abs(spritePos - currentCRTCol) <= 1
        then Pixel.Lit
        else Pixel.Dark
      currentCRTRow :+= newPixel
      if cycle % 40 == 0
      then {
        crt :+= currentCRTRow.toList
        currentCRTRow = Queue()
      }

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day10(): Unit =
    val program = IO
      .readFileLines("y2022/day10.txt")
      .map(parseInstruction)
      .toList
    println(solve1(program))
    println(solve2(program))

  def parseInstruction(line: String): Instruction =
    import Instruction.*
    line.split(" ") match
      case Array("noop")    => NoOp
      case Array("addx", x) => AddX(x.toInt)
      case _                => throw IllegalArgumentException("Invalid instruction!")
