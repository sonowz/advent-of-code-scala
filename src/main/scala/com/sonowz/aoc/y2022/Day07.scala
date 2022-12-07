package com.sonowz.aoc.y2022

import com.sonowz.aoc.common.IO

import scala.util.chaining.scalaUtilChainingOps

object AoCDay07:

  ///////////////////////
  // Type Declarations //
  ///////////////////////

  case class CommandResult(cmd: Command, result: Result)

  enum Command:
    case CD(path: String)
    case LS

  type Result = String

  enum FileSystem:
    case Dir(dirname: String, subdir: Map[String, FileSystem])
    case File(filename: String, size: Int)

  object FileSystem:
    def newDir(dirname: String): FileSystem = Dir(dirname, Map())

  extension (fs: FileSystem)

    def getSubdir: Map[String, FileSystem] =
      fs match
        case FileSystem.Dir(_, subdir) => subdir
        case FileSystem.File(_, _)     => throw IllegalArgumentException("Invalid cursor!")

    def updateFs(cursor: Seq[String])(updateFn: FileSystem => FileSystem): FileSystem =
      import FileSystem.*
      cursor match
        case Nil => updateFn(fs)
        case dir :: dirs =>
          val newSubdir = fs.getSubdir.updatedWith(dir) {
            case None        => Some(newDir(dir).updateFs(dirs)(updateFn))
            case Some(subFs) => Some(subFs.updateFs(dirs)(updateFn))
          }
          fs match {
            case Dir(dirname, _) => Dir(dirname, newSubdir)
            case _               => throw IllegalArgumentException("Corrupted filesystem!")
          }

  ////////////
  // Part 1 //
  ////////////

  def solve1(commandResults: Seq[CommandResult]): Int =
    buildFileSystem(commandResults)
      .pipe(extractDirectorySizes)
      .filter(_ <= 100_000)
      .sum

  def buildFileSystem(commandResults: Seq[CommandResult]): FileSystem =
    import Command.*
    import FileSystem.*
    def foldFn = (state: (FileSystem, List[String]), commandResult: CommandResult) =>
      val (fs, cursor) = state
      commandResult.cmd match
        case CD("/")  => (fs, List())
        case CD("..") => (fs, cursor.init)
        case CD(path) => (fs, cursor :+ path)
        case LS =>
          val newFs = fs.updateFs(cursor)(fs_ => updateFsWithLsResult(fs_, commandResult.result))
          (newFs, cursor)
    val initState = (newDir("/"), List())
    commandResults.foldLeft(initState)(foldFn)._1

  def updateFsWithLsResult(fs: FileSystem, result: Result): FileSystem =
    import FileSystem.*

    // Since 'ls' result is always same in same directory, no update is needed
    if fs.getSubdir.nonEmpty then return fs

    val subdir = result
      .split("\n")
      .map {
        _.split(" ") match
          case Array("dir", dirname) => dirname -> newDir(dirname)
          case Array(size, filename) => filename -> File(filename, size.toInt)
      }
      .toMap

    fs match
      case Dir(dirname, _) => Dir(dirname, subdir)
      case _               => throw IllegalArgumentException("Invalid call!")

  def extractDirectorySizes(fs: FileSystem): Seq[Int] =
    def go(fs: FileSystem, acc: Seq[Int]): (Seq[Int], Int) =
      fs match
        case FileSystem.File(_, size) => (acc, size)
        case FileSystem.Dir(_, subdir) =>
          val (newAcc, newSize) = subdir.values.foldLeft((acc, 0)) { (state, fs_) =>
            val (acc_, size_) = state
            val (newAcc, size) = go(fs_, acc_)
            val newSize = size_ + size
            (newAcc, newSize)
          }
          (newSize +: newAcc, newSize)
    go(fs, List())._1

  ////////////
  // Part 2 //
  ////////////

  def solve2(commandResults: Seq[CommandResult]): Int =
    val directorySizes = buildFileSystem(commandResults)
      .pipe(extractDirectorySizes)
    val rootSize = directorySizes.max
    directorySizes
      .filter(rootSize - _ <= 40_000_000)
      .min

  ////////////////////
  // Main & Parsing //
  ////////////////////

  @main
  def Day07(): Unit =
    val templates = IO
      .readFile("y2022/day07.txt")
      .pipe(parseCommandResults)
    println(solve1(templates))
    println(solve2(templates))

  def parseCommandResults(str: String): Seq[CommandResult] =
    str
      .drop(2)
      .split("""\r?\n\$ """)
      .map(parseCommandResult)

  def parseCommandResult(str: String): CommandResult =
    str.take(2) match
      case "cd" => CommandResult(Command.CD(str.drop(3)), "")
      case "ls" => CommandResult(Command.LS, str.drop(3))
