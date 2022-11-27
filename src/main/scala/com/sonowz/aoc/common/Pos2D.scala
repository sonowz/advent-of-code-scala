package com.sonowz.aoc.common

trait Pos2D[T] extends Ordering[T]:
  extension (p: T) def to2DTuple(): (Int, Int)
  extension (tuple: (Int, Int)) def from2DTuple(): T

object Pos2D:
  given (using ord: Ordering[(Int, Int)]): Pos2D[(Int, Int)] with
    extension (p: (Int, Int)) override def to2DTuple(): (Int, Int) = p
    extension (tuple: (Int, Int)) override def from2DTuple(): (Int, Int) = tuple
    override def compare(x: (Int, Int), y: (Int, Int)): Int = ord.compare(x, y)
