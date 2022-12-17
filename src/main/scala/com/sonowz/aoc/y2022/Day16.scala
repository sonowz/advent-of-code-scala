package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.collection.mutable
import scala.util.chaining.given
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.lexical.StdLexical
import scalax.collection.mutable.Graph
import scalax.collection.GraphPredef.Param
import scalax.collection.edge.WUnDiEdge

object AoCDay16:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Valve(name: String, flowRate: Int, leadTo: Seq[String]) {
    override def hashCode(): Int = name.hashCode()
  }

  type ValveGraph = Graph[Valve, WUnDiEdge]

  ////////////
  // Part 1 //
  ////////////

  def solve1(valves: Seq[Valve]): Int =
    val startValve = valves.find(_.name == "AA").get
    val valveGraph = compressGraph(constructGraph(valves), startValve)
    getMaxPressure(valveGraph, startValve, 30)

  def constructGraph(valves: Seq[Valve]): ValveGraph =
    val valveMap: Map[String, Valve] = valves.map(v => (v.name, v)).toMap
    val edges = valves.flatMap { v =>
      v.leadTo
        .map(l => WUnDiEdge(v, valveMap(l))(1.0))
    }
    Graph.from(valves, edges)

  // Delete all zero-flow valves, and make clique
  def compressGraph(graph: ValveGraph, startValve: Valve): ValveGraph =
    val valvesWithFlow = graph.nodes.filter(_.toOuter.flowRate > 0) ++ Set(graph.get(startValve))
    val edges = valvesWithFlow.toSeq.flatMap { v =>
      valvesWithFlow
        .filter(_ != v)
        .map(u => WUnDiEdge(v.toOuter, u.toOuter)(v.shortestPathTo(u).get.weight))
    }
    Graph.from(valvesWithFlow.map(_.toOuter), edges)

  // Memoization decorator
  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  def getMaxPressure(graph: ValveGraph, startValve: Valve, time: Int): Int =

    lazy val go: ((Set[Valve], Valve, Int)) => Int = memoize {
      case (visited, valve, _) if visited(valve) => 0
      case (visited, valve, remainingTime_) =>
        val remainingTime = remainingTime_ - 1

        val openedFlow = valve.flowRate * remainingTime
        val visits = graph.get(valve).outgoing.flatMap { e =>
          val u = e.nodes.filter(_ != valve).head
          val weight = e.weight.toInt
          val nextRemainingTime = remainingTime - weight
          if nextRemainingTime > 0
          then Some(go(visited ++ Set(valve), u, nextRemainingTime))
          else None
        }
        openedFlow + visits.maxOption.getOrElse(0)
    }

    // time + 1: Compensate for extra time to open startValve
    go(Set(), startValve, time + 1)

  ////////////
  // Part 2 //
  ////////////

  // Note: this takes about 2 minutes
  def solve2(valves: Seq[Valve]): Int =
    val startValve = valves.find(_.name == "AA").get
    val valveGraph = compressGraph(constructGraph(valves), startValve)

    def nodeFilter(valves: Seq[Valve])(param: Param[Valve, WUnDiEdge]): Boolean =
      val valveSet = valves.toSet
      param match
        case valveGraph.InnerNode(n) => valveSet(n) || n == startValve
        case valveGraph.InnerEdge(e) => e.nodeSeq.forall(n => valveSet(n) || n == startValve)
        case _                       => throw new IllegalArgumentException("Unexpected graph component")

    val nonStartValves = valveGraph.nodes.toOuter.filter(_ != startValve).toSeq
    splitValves(nonStartValves).map { (firstValves, secondValves) =>
      getMaxPressure(valveGraph.filter(nodeFilter(firstValves)), startValve, 26)
        + getMaxPressure(valveGraph.filter(nodeFilter(secondValves)), startValve, 26)
    }.max

  // All cases of splitting valves to (first, second) such that first.length >= second.length
  def splitValves(valves: Seq[Valve]): Iterator[(Seq[Valve], Seq[Valve])] =
    val secondLengths = valves.length / 4 to valves.length / 2
    secondLengths.flatMap { secondLength =>
      valves
        .combinations(secondLength)
        .map { second =>
          val first = valves.filter(v => second.forall(_.name != v.name))
          (first, second)
        }
    }.iterator

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day16(): Unit =
    val valves = IO
      .readFile("y2022/day16.txt")
      .pipe(ValveParser.apply)
    println(solve1(valves))
    println(solve2(valves))

  object ValveParser extends StdLexical with RegexParsers:

    override def skipWhitespace: Boolean = true

    def integer: Parser[Int] = """\d+""".r ^^ (_.toInt)

    def apply(input: String): Seq[Valve] =
      parseAll(valves, input).get

    def valves: Parser[Seq[Valve]] =
      (valveName ~ flowRate ~ leadTo).* ^^ (_.map { case valveName ~ flowRate ~ leadTo =>
        Valve(valveName, flowRate, leadTo)
      })

    def valveName: Parser[String] = "Valve" ~> """\w+""".r

    def flowRate: Parser[Int] = "has flow rate=" ~> integer <~ ";"

    def leadTo: Parser[Seq[String]] = "tunnels? leads? to valves?".r ~> rep1sep("""\w+""".r, ", ")
