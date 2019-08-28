package com.tsurovec

import org.scalatest.{FlatSpec, Matchers}

class TranslatorTests extends FlatSpec with Matchers {
  "removeComments" should "trim lines" in {
    Translator.removeComments(List("    a     ")) should be (List("a"))
  }

  "removeComments" should "remove commented lines" in {
    Translator.removeComments(List("a", "    // commneted out", "b")) should be (List("a", "b"))
  }

  "removeComments" should "remove comments from lines" in {
    Translator.removeComments(List("a", "   a2 // commented out", "b")) should be (List("a", "a2", "b"))
  }

  "removeComments" should "remove blank lines" in {
    Translator.removeComments(List("a", "  ", "", "b")) should be (List("a", "b"))
  }

  "removeSymbolicReferences" should "correctly substitute references for well-known symbols" in {
    // R0-R15
    Translator.removeSymbolicReferences((0 to 15).map(i => s"@R$i").toList) should be ((0 to 15).map(i => s"@$i"))

    // SCREEN
    //Translator.removeSymbolicReferences(List("@SCREEN")) should be (List(s"@${Translator.SCREEN}"))

    // KBD
    //Translator.removeSymbolicReferences(List("@KBD")) should be (List(s"@${Translator.KBD}"))
  }

  "removeSymbolicReferences" should "remove labels" in {
    Translator.removeSymbolicReferences(List("a", "(main)", "(main2)", "b", "@main", "@main2", "@main3", "c", "(main3)")) should be
    (List("a", "b", "@1", "@1", "@6", "c"))
  }

  "removeSymbolicReferences" should "generate memory references" in {
    Translator.removeSymbolicReferences(List("@unknown1", "@unknown2")) should be (List("@16", "@17"))
  }

  // instruction translation
  // A instruction
  "@1234" should "load 1234 to A" in {
    Translator.toInstruction("@1234").tomach should be (1234)
  }

  // C instructions
  // computation
  "computation" should "bla" in {
    // a = 0

    def ac(s: Short): Short = (s & 0x1FC0 >> 6).toShort


    def ass(s: String, exp: Short) = ac(Translator.toInstruction(s).tomach) should be (exp)

    ass("0", 0x2A)
    ass("1", 0x3F)
    ass("-1", 0x3A)
    ass("D", 0x0C)
    ass("A", 0x30)

    // a = 1
  }

  "get acbits" should "" in {
    Translator.getAcBits("A") should be (0x0C00)

  }

  "tes" should "?" in {
    def run(s: String) = {
      println(s)
      println(Translator.toInstruction(s).human)

    }

    run("D=D-A")
    /*run("D;JEQ")
    run("@7")
    run("MD=M+1")
    run("0;JMP")*/


  }
  // assignment
  "assignments" should " " in {
    val x = Translator.toInstruction("D=D-A").tomach
    val i: Int = 0 | x
    x should be(0xE4D0.toShort)

  }

  // jumps

}
