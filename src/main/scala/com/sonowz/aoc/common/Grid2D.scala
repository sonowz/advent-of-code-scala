package com.sonowz.aoc.common

import scala.util.Try
import com.sonowz.aoc.common.Pos2D.given

trait Grid2D[G]:

  type Item

  extension (grid: G)

    def size2D[P: Pos2D]: P

    /** Returns string with newlines */
    def mkString2D(): String

    def get2DOption[P: Pos2D](idx: P): Option[Item]

    def get2D[P: Pos2D](idx: P): Item =
      grid.get2DOption(idx) match {
        case Some(item) => item
        case None       => throw IndexOutOfBoundsException(idx.toString)
      }

object Grid2D:

  given [T]: Grid2D[Seq[Seq[T]]] with

    type Item = T

    extension (grid: Seq[Seq[T]])

      override def size2D[P: Pos2D]: P =
        val tuple = (grid.size, grid.applyOrElse(0, Seq()).size)
        tuple.from2DTuple()

      override def mkString2D(): String =
        grid.map(_.mkString("")).mkString("\n")

      override def get2DOption[P: Pos2D](idx: P): Option[T] =
        val (i, j) = idx.to2DTuple()
        grid.lift(i).flatMap(_.lift(j))

  given [T]: Grid2D[Array[Array[T]]] with

    type Item = T

    extension (grid: Array[Array[T]])

      override def size2D[P: Pos2D]: P =
        val tuple = (grid.length, grid.headOption.map(_.length).getOrElse(0))
        tuple.from2DTuple()

      override def mkString2D(): String =
        grid.map(_.mkString("")).mkString("\n")

      override def get2DOption[P: Pos2D](idx: P): Option[T] =
        val (i, j) = idx.to2DTuple()
        grid.lift(i).flatMap(_.lift(j))

  given [K: Pos2D, V]: Grid2D[Map[K, V]] with

    type Item = V

    private def getBounds(grid: Map[K, V]): ((Int, Int), (Int, Int)) =
      val xs = grid.keys.map(_.to2DTuple()._1)
      val ys = grid.keys.map(_.to2DTuple()._1)
      val getBound = (xs: Iterable[Int]) => (xs.min, xs.max)
      val (minX, maxX) = getBound(xs)
      val (minY, maxY) = getBound(ys)
      ((minX, minY), (maxX, maxY))

    extension (grid: Map[K, V])

      override def size2D[P: Pos2D]: P =
        val ((minX, minY), (maxX, maxY)) = getBounds(grid)
        (maxX - minX + 1, maxY - minY + 1).from2DTuple()

      def mkString2D(default: String): String =
        val ((minX, minY), (maxX, maxY)) = getBounds(grid)
        val mkStringLine = (x: Int, y: Int) =>
          val pos = (x, y).from2DTuple()
          grid.get2DOption(pos).map(_.toString).getOrElse(default)
        val mkStringGrid = (x: Int) => (minY to maxY).map(mkStringLine(x, _)).mkString("")
        (minX to maxX).map(mkStringGrid).mkString("")

      override def mkString2D(): String = grid.mkString2D(" ")

      override def get2DOption[P: Pos2D](idx: P): Option[V] =
        val keyTuple: (Int, Int) = idx.to2DTuple()
        val key: K = summon[Pos2D[K]].from2DTuple(keyTuple)()
        grid.get(key)
