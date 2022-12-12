package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.given
import scala.util.Try
import scalax.collection.Graph
import scalax.collection.edge.WDiEdge

object AoCDay12:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class Heightmap(graph: HeightGraph, heights: Map[Pos, Int], start: Pos, end: Pos)
  type HeightGraph = Graph[Pos, WDiEdge]
  type Pos = (Int, Int)

  ////////////
  // Part 1 //
  ////////////

  def solve1(heightmap: Heightmap): Int =
    val graph = heightmap.graph
    val startNode = graph.get(heightmap.start)
    val endNode = graph.get(heightmap.end)
    startNode
      .shortestPathTo(endNode)
      .getOrElse(throw IllegalArgumentException("No path from start to end!"))
      .edges
      .map(_.weight)
      .sum
      .toInt

  ////////////
  // Part 2 //
  ////////////

  def solve2(heightmap: Heightmap): Int =
    val graph = heightmap.graph
    val startNodes = graph.nodes.filter(p => heightmap.heights(p) == 0)
    val endNode = graph.get(heightmap.end)
    def shortestPathLength(startNode: graph.NodeT): Option[Int] =
      startNode
        .shortestPathTo(endNode)
        .map(
          _.edges
            .map(_.weight)
            .sum
            .toInt
        )
    startNodes.flatMap(shortestPathLength).min

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day12(): Unit =
    val heightmap = IO
      .readFileLines("y2022/day12.txt")
      .pipe(parseHeightmap)
    println(solve1(heightmap))
    println(solve2(heightmap))

  def parseHeightmap(linesSeq: Seq[String]): Heightmap =
    val lines = linesSeq.toIndexedSeq
    val mapSize = (lines.head.length, lines.length)
    var nodes: Seq[Pos] = Seq()
    var edges: Seq[WDiEdge[Pos]] = Seq()
    var heights: Map[Pos, Int] = Map()
    var start: Pos = null
    var end: Pos = null

    def getHeight(p: Pos): Option[Int] =
      Try(lines(p._2)(p._1)).toOption.map {
        case 'S' => 0
        case 'E' => 25
        case c   => c - 'a'
      }

    def tryMakeEdge(u: Pos, v: Pos): Unit =
      for {
        weightU <- getHeight(u)
        weightV <- getHeight(v)
        if weightV - weightU <= 1
      } do {
        edges +:= WDiEdge(u, v)(1.0)
      }

    for {
      y <- 0 until mapSize._2
      x <- 0 until mapSize._1
    } do {
      val p = (x, y)
      val pRight = (x + 1, y)
      val pDown = (x, y + 1)
      lines(y)(x) match
        case 'S' => start = p
        case 'E' => end = p
        case _   =>
      nodes +:= p
      heights = heights.updated(p, getHeight(p).get)
      tryMakeEdge(p, pRight)
      tryMakeEdge(pRight, p)
      tryMakeEdge(p, pDown)
      tryMakeEdge(pDown, p)
    }
    Heightmap(Graph.from(nodes, edges), heights, start, end)
