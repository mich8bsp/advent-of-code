package aoc2022

import scala.annotation.tailrec

object Day7 {

  sealed trait FileDescriptor
  case class SingleFileDescriptor(name: String, size: Long) extends FileDescriptor
  case class DirDescriptor(name: String) extends FileDescriptor

  sealed trait Command
  case class LsCommand(files: List[FileDescriptor]) extends Command
  case class CdCommand(dirName: String) extends Command

  sealed trait FileSystemNode {
    val name: String
    def size: Long
    var parent: Option[DirFileSystemNode] = None
  }

  case class SingleFileSystemNode(name: String, fileSize: Long) extends FileSystemNode {
    val size: Long = fileSize
  }

  case class DirFileSystemNode(name: String) extends FileSystemNode {
    var children: List[FileSystemNode] = Nil
    lazy val size: Long = children.map(_.size).sum
  }

  def buildFileSystem(commands: List[Command]): DirFileSystemNode = {
    val root: DirFileSystemNode = DirFileSystemNode("/")
    var currentDir = root
    commands.foreach {
      case CdCommand("/") =>
        currentDir = root
      case CdCommand("..") =>
        currentDir = currentDir.parent.getOrElse(root)
      case CdCommand(fileName) =>
        currentDir = currentDir.children.collectFirst {
          case d@DirFileSystemNode(`fileName`) => d
        }.getOrElse(throw new Exception(s"File $fileName not found"))
      case LsCommand(files) =>
        if (currentDir.children.isEmpty) {
          val subFileNodes = files.map {
            case DirDescriptor(dirName) => DirFileSystemNode(dirName)
            case SingleFileDescriptor(fileName, fileSize) => SingleFileSystemNode(fileName, fileSize)
          }
          subFileNodes.foreach(subFile => subFile.parent = Some(currentDir))
          currentDir.children = subFileNodes
        }
    }
    root
  }

  @tailrec
  def parseInput(lines: List[String], output: List[Command] = Nil): List[Command] = lines match {
    case Nil => output
    case first :: rest if first.startsWith("$ cd") =>
      val parsedCommand: CdCommand = CdCommand(first.split(" ")(2))
      parseInput(rest, output :+ parsedCommand)
    case "$ ls" :: rest =>
      val outputLines = rest.takeWhile(!_.startsWith("$"))
      val parsedOutput: List[FileDescriptor] = outputLines.map { line =>
        val Array(desc, name) = line.split(" ")
        desc match {
          case "dir" => DirDescriptor(name)
          case _ => SingleFileDescriptor(name, desc.toLong)
        }
      }
      val parsedCommand = LsCommand(parsedOutput)
      parseInput(rest.drop(outputLines.size), output :+ parsedCommand)
  }

  def traverseFs(fs: DirFileSystemNode, f: FileSystemNode => Boolean): List[FileSystemNode] = {
    val subFilesTraverseRes: List[FileSystemNode] = fs.children.flatMap {
      case child: DirFileSystemNode => traverseFs(child, f)
      case _ => Nil
    }

    List(fs).filter(f) ++ subFilesTraverseRes
  }

  def solveA(fs: DirFileSystemNode): Long = {
    traverseFs(fs, file => file.size <= 100000)
      .map(_.size)
      .sum
  }

  def solveB(fs: DirFileSystemNode): Long = {
    val totalSpace = 70000000L
    val usedSpace = fs.size
    val unusedSpace = totalSpace - usedSpace
    val requiredSpace = 30000000L
    val spaceToFree = requiredSpace - unusedSpace

    traverseFs(fs, file => file.size >= spaceToFree)
      .map(_.size)
      .min
  }

  def main(args: Array[String]): Unit = {
    val commands: List[Command] = parseInput(readFileLines[String](7))
    val fsRoot = buildFileSystem(commands)
    println(solveA(fsRoot))
    println(solveB(fsRoot))
  }
}
