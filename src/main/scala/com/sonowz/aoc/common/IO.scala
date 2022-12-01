package com.sonowz.aoc.common

import java.io.File
import scala.io.Source
import scala.util.Using

object IO {
  def readFileLines(path: String): Seq[String] =
    Using(Source.fromFile("src/main/resources/" + path)) { _.getLines().toSeq }.get
}
