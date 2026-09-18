/*
   Copyright 2026 Ondrej Lhotak. All rights reserved.

   Permission is granted for private study use by students registered in
   CS 241E in the Fall 2026 term.

   The contents of this file may not be published, in whole or in part,
   in print or electronic form.

   The contents of this file may be included in work submitted for CS
   241E assignments in Fall 2026. The contents of this file may not be
   submitted, in whole or in part, for credit in any other course.
*/
package cs241e.assignments

import cs241e.mips.*
import cs241e.Utils.*
import State.*
import Assembler.*
import cs241e.assignments.ProgramRepresentation.*

import scala.annotation.tailrec

/** This is a tracer/debugger for the MIPS machine that is provided for your convenience in debugging your
  * MIPS programs. Feel free to modify and improve the debugger for your needs.
  */

object Debugger {
  /** Attempt to disassemble a word into a MIPS instruction, and return in the form of a `String`. */
  def disassemble(instruction: Word): String = {
    def invalidInstruction = decodeSigned(instruction).toString

    val List(op, sBits, tBits, iBits) = instruction.splitAt(List(6, 5, 5))
    val s = decodeUnsigned(sBits)
    val t = decodeUnsigned(tBits)
    val i = decodeSigned(iBits)
    op match {
      case Bits("000000") =>
        val List(dBits, zeros, function) = iBits.splitAt(List(5, 5))
        val d = decodeUnsigned(dBits)
        if (zeros != Bits("00000")) invalidInstruction
        else function match {
          case Bits("100000") => s"add $$$d, $$$s, $$$t"
          case Bits("100010") => s"sub $$$d, $$$s, $$$t"
          case Bits("011000") if d == 0 => s"mult $$$s, $$$t"
          case Bits("011001") if d == 0 => s"multu $$$s, $$$t"
          case Bits("011010") if d == 0 => s"div $$$s, $$$t"
          case Bits("011011") if d == 0 => s"divu $$$s, $$$t"
          case Bits("010000") if s == 0 && t == 0 => s"mfhi $$$d"
          case Bits("010010") if s == 0 && t == 0 => s"mflo $$$d"
          case Bits("010100") if s == 0 && t == 0 => s"lis $$$d"
          case Bits("101010") => s"slt $$$d, $$$s, $$$t"
          case Bits("101011") => s"sltu $$$d, $$$s, $$$t"
          case Bits("001000") if t == 0 && d == 0 => s"jr $$$s"
          case Bits("001001") if t == 0 && d == 0 => s"jalr $$$s"
          case _ => invalidInstruction
        }

      case Bits("100011") => s"lw $$$t, $i($$$s)"
      case Bits("101011") => s"sw $$$t, $i($$$s)"
      case Bits("000100") => s"beq $$$s, $$$t, $i"
      case Bits("000101") => s"bne $$$s, $$$t, $i"
      case _ => invalidInstruction
    }
  }

  /** Associates optional strings (such as labels or comments) with each address.
    * The strings are printed during debugging when execution reaches the address.
    */
  type DebugTable = Map[Word, Seq[Comment|Label]]

  /** Runs the CPU starting from `state`, printing information about the CPU state at
    * each step. Whenever execution reaches an address that appears in the `debugTable`,
    * the corresponding string (comment/label) is printed as well.
    */
  def debug(state: State, debugTable: DebugTable = Map.empty): State = {
    val id = new InteractiveDebugger(state, debugTable) { override def isAtBreakPoint: Boolean = false }
    id.run
    id.state
  }

  /** Instantiate an interactive debugger that supports pausing execution at breakpoints,
    * inspecting current state, and single-stepping. See the methods in the
    * `InteractiveDebugger` class below.
    */
  def interactive(state: State, debugTable: DebugTable = Map.empty): InteractiveDebugger =
    new InteractiveDebugger(state, debugTable)

  class InteractiveDebugger(initialState: State, debugTable: DebugTable) {
    var state: State = initialState
    def pc = state.reg(PC)
    def isDoneExecuting = pc == CPU.terminationPC

    /** Defines whether the execution is in a state where the debugger should pause
      * execution. By default, this is when the program counter points to an
      * instruction with a comment that starts with "BREAK". Feel free to modify
      * this method to make the debugger pause in other execution states.
      */
    def isAtBreakPoint: Boolean = debugTable.getOrElse(pc, Seq()).exists {
      case Comment(msg) => msg.startsWith("BREAK")
      case _ => false
    }

    /** This method is called at each step of execution to print the current state
      * of the MIPS machine. By default, it prints the values of two of the registers,
      * any comments and labels at the current location in the program counter, and
      * the instruction about to be executed.
      *
      * Feel free to modify this method to print whatever state you need to debug.
      */
    def showCurrentState: Unit = {
      def isAtLISInstruction: Boolean = disassemble(state.mem(pc)).startsWith("lis ")

      regs(3, 30)

      if (debugTable.isDefinedAt(pc)) println(debugTable(pc).mkString("\n"))

      if(isDoneExecuting) println("EXECUTION FINISHED")
      else mem(pc, words = if (isAtLISInstruction) 2 else 1)
    }

    /** Prints the current values of (all or a subset of) registers. */
    def regs: Unit = regs(1 to 31 :_*)
    def regs(regsToPrint: Int*): Unit = {
      val string = regsToPrint.map(reg => s"$reg: ${prettyPrintWord(state.reg(reg))}   ").foldLeft("")(_ + _)
      println(string)
    }

    /** Prints the current values of words in memory starting at the specified address. */
    def mem(address: String, words: Int = 1): Unit = mem(Word(address), words)
    def mem(address: Word): Unit = mem(address, 1)
    def mem(address: Word, words: Int): Unit = dumpMem(state, address, words)

    /** Run steps of the CPU until it finishes executing or reaches a state in which `isAtBreakPoint`
      * returns `true`.
      */
    def run: Unit = run(Integer.MAX_VALUE)
    @tailrec final def run(maxSteps: Int): Unit = {
      if(maxSteps > 0 && !isDoneExecuting) {
        state = CPU.step(state)
        showCurrentState
        if(!isAtBreakPoint) run(maxSteps - 1)
      }
    }

    /** Execute one step of the CPU. */
    def step: Unit = run(1)

    showCurrentState
  }

  /** Print a word with spaces between each group of 8 bits to aid readability. */
  def prettyPrintWord(w: Word): String = w.toString().sliding(8, 8).mkString(" ")

  /** Prints the current values of a number of words of memory in the given state starting
    * at the given address. */
  def dumpMem(state: State, address: Word, words: Int = 1): Unit = {
    if (words > 0) {
      val word = state.mem(address)
      println(s"${prettyPrintWord(address)}: ${prettyPrintWord(word)} // ${disassemble(word)}")
      dumpMem(state, Word(encodeUnsigned(decodeUnsigned(address) + 4)), words - 1)
    }
  }
}