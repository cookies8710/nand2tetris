package com.tsurovec

import java.io.FileWriter

object Translator {
  type MachineWord = Short
  type Source = List[String]

  def removeComments(program: Source): Source = {
    def deleteComment(line: String): String = {
      val i = line.indexOf("//")
      if (i < 0)
        line
      else
        line.substring(0, i)
    }

    program.map(deleteComment).map(_.trim).filterNot(_.isEmpty)
  }

  def removeSymbolicReferences(program: Source): Source = {
    def getLabel(s: String) = {
      s.substring(1, s.length - 1)
    }

    def isLabel(s: String) = {
      s.startsWith("(") && s.endsWith(")")
    }

    var symbollessProgram: List[String] = List()
    var pc = 0
    var newVariableAddress = 16

    // init with well-known symbols
    var symbols: Map[String, Int] = Map(
      "SCREEN" -> 0x4000,
      "KBD"    -> 0x600,
      "SP"     -> 0,
      "LCL"    -> 1,
      "ARG"    -> 2,
      "THIS"   -> 3,
      "THAT"   -> 4) ++ (0 to 15).map(i => s"R$i" -> i).toMap

    // first pass - add labels to symbols along with the ROM address they're pointing to
    program.indices.foreach { i =>
      if (isLabel(program(i)))
        symbols += getLabel(program(i)) -> pc
      else
        pc += 1
    }

    def subSymbols(line: String, newVarAddr: Int, sym: Map[String, Int]): (String, Int, Map[String, Int]) = {
      if (line.startsWith("@") && !line.substring(1).forall(_.isDigit)) {
        val lab = line.substring(1)
        if (sym.contains(lab))
          (s"@${sym(lab)}", newVarAddr, sym)
        else
          (s"@$newVarAddr", newVarAddr + 1, sym + (lab -> newVarAddr))
      }
      else
        (line, newVarAddr, sym)
    }

    // second pass - substitute all address loads by either address from symbols or generate a new variable symbol
    program.filterNot(isLabel).foreach { line =>
      val (newLine, newVarAddr, sym) = subSymbols(line, newVariableAddress, symbols)
      symbollessProgram = newLine :: symbollessProgram
      newVariableAddress = newVarAddr
      symbols = sym
    }
    symbollessProgram.reverse
  }

  abstract class Instruction(mnemo: String) {
    def tomach: Short
    def tohack = bin(tomach)
    def human: String
    def bin(s: Short): String = {
      (0 to 15).reverse.map(i => (s & (scala.math.pow(2, i).toInt)) > 0).map(if (_) '1' else '0').mkString
    }
  }

  case class AInstruction(mnemo:String, address: Short) extends Instruction(mnemo) {
    override def tomach: Short = (address & 0x7fff).toShort

    override def human: String = s"$mnemo\n${bin(tomach).grouped(4).mkString(" ")}"
  }

  case class CInstruction(m: String, ac: Short, dst: Short, jmp: Short) extends Instruction(m) {
    override def tomach: MachineWord =( 0xe000 | ac | dst | jmp).toShort


    override def human: String = {
      s"$m\n          a c1 c2 c3 c4 c5 c6 d1 d2 d3 j1 j2 j3\n"+
      bin(tomach).map(x => s" $x ").mkString("")
    }
  }

  def getAcBits(expr: String): MachineWord = {
    def shift(a: Short): MachineWord = (a << 6).toShort

    val base: Short = expr match {
      case "0" => 0x2A
      case "1" => 0x3F
      case "-1" => 0x3A
      case "A" => 0x30
      case "M" => 0x70
      case "D" => 0x0C
      case "!A" => 0x31
      case "!M" => 0x71
      case "!D" => 0x0D
      case "-A" => 0x33
      case "-M" => 0x73
      case "-D" => 0x0F
      case "D+1" => 0x1F
      case "A+1" => 0x37
      case "M+1" => 0x77
      case "D-1" => 0x0E
      case "A-1" => 0x32
      case "M-1" => 0x72
      case "D+A" => 0x02
      case "D+M" => 0x42
      case "D-A" => 0x13
      case "D-M" => 0x53
      case "A-D" => 0x07
      case "M-D" => 0x47
      case "D&A" => 0x00
      case "D&M" => 0x40
      case "D|A" => 0x15
      case "D|M" => 0x55
    }

    shift(base)
  }

  def getDestinationBits(destination: String): Short = {
    var r: Short = 0
    if (destination != null && destination.contains('M')) r = (r | 0x1).toShort
    if (destination != null && destination.contains('D')) r = (r | 0x2).toShort
    if (destination != null && destination.contains('A')) r = (r | 0x4).toShort
    (r<<3).toShort
  }

  def getJumpBits(j: String): Short =
    j match {
      case "null" => 0
      case "JGT" => 1
      case "JEQ" => 2
      case "JGE" => 3
      case "JLT" => 4
      case "JNE" => 5
      case "JLE" => 6
      case "JMP" => 7
    }

  def toInstruction(line: String): Instruction = {
    val aInstructionR = "@(\\d+)".r
    val cInstructionR = "([AMD]+=)?([AMD\\+\\-\\&\\|01!]+)".r
    val jumpR = "([AMD0]);(null|J..)".r

    line match {
      case aInstructionR(address) => AInstruction(line, address.toShort)
      case cInstructionR(dest, oper) =>
        CInstruction(line, getAcBits(oper), getDestinationBits(dest), 0)
      case jumpR(expression, jmp) =>
        val ac = getAcBits(expression)
        val jb = getJumpBits(jmp)
        CInstruction(line, ac, 0, jb)
      case _ => throw new RuntimeException("Unknown instruction: $line")
    }
  }

  def translate(program: Source): List[Instruction] = removeSymbolicReferences(removeComments(program)).map(toInstruction)
}

object Assembler extends App {
  def emit(inFilename: String): Unit = {
    val inFilenameR = "(.*)\\.asm".r
    val outFilename = inFilename match {
      case inFilenameR(filename) => s"$filename.hack"
      case _ => throw new RuntimeException(s"Input file must have an .asm extension; $inFilename doesn't comply")
    }

    val lines = scala.io.Source.fromFile(inFilename).getLines().toList

    val translated = Translator.translate(lines)

    translated.map(_.human).zipWithIndex.map { case (s, i) => s"$i: $s" } foreach(println)

    println(s"\n\nWriting to $outFilename")
    val fw = new FileWriter(outFilename)
    translated.map(_.tohack).map(_ + "\n").foreach(fw.write)
    fw.close()
  }
  args.foreach(emit)
}
