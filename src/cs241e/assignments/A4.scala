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
import Assembler.*
import Transformations.*
import ProgramRepresentation.*
import CodeBuilders.*
import A1.*

object A4 {
  /** This is an enhanced version of the loadAndRun method from Assignment 1.
    *
    * It loads the give machine language code into memory, writes the specified values to registers 1 and 2,
    * and then runs the CPU on the code that was loaded. The debugger can be invoked by passing the argument
    * debug = true.
    */
  def loadAndRun(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero, debug: Boolean = false): State = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    if(debug) Debugger.debug(initialState, code.debugTable)
    else CPU.run(initialState)
  }
  
  def interactiveDebugger(code: MachineCode, register1: Word = Word.zero, register2: Word = Word.zero): Debugger.InteractiveDebugger = {
    val initialState =
      setMem(code.words)
        .setReg(1, register1)
        .setReg(2, register2)
    Debugger.interactive(initialState, code.debugTable)
  }

  /** A utility method that takes two sequences of words representing `code` and an `array`,
    * loads both into memory, and sets register 1 to the address of the beginning
    * of the array and register 2 to its length in words.
    */
  def loadCodeAndArray(code: Seq[Word], array: Seq[Word]): State = {
    val arrayAddress: Word = Word(encodeUnsigned(code.size * 4))
    val arrayLength: Word = Word(encodeUnsigned(array.size))
    val loadedCode: State = setMem(code)
    val loadedArray: State = setMem(array, loadedCode, arrayAddress)
    loadedArray.setReg(1, arrayAddress).setReg(2, arrayLength)
  }

  /** A utility method that loads code and an array into memory and runs the code.
    * The debugger can be invoked by passing the argument debug = true.
    */
  def loadAndRunArray(code: MachineCode, array: Seq[Word], debug: Boolean = false): State = {
    val state = loadCodeAndArray(code.words, array)
    if(debug) Debugger.debug(state, code.debugTable)
    else CPU.run(state)
  }

  /** Code that loads the constant `i` into `Reg.result`. */
  private def const(i: Int): Code = block(LIS(Reg.result), Word(encodeSigned(i)))

  /** Code that computes the address of element `index` of the array whose address is in `Reg.input1`,
    * i.e. `Reg.input1 + 4 * index`, leaving it in `Reg.result`.
    *
    * This is a `def` so that each use builds fresh `Code` (with fresh temporary `Variable`s); reusing the same
    * `Scope` twice in a program would make `compilerA4` see the same variable twice.
    */
  
  private def elementAddr(index: Code):Code=
    binOp(ADD(Reg.result, Reg.input1,Reg.zero), plus, binOp(index, times, const(4)))
    
  /** Code that prints the character whose ASCII value is computed by `expr`. */
  private def printChar(expr: Code): Code = 
    
    
    
    
    
    
    assignToAddr(block(LIS(Reg.result), CPU.printAddr), expr)

  
  
  
  
  
  
  
  
  
  
  
  /** Register 1 holds the address of the beginning of an array of 32-bit integers.
    * Register 2 holds the number of elements in the array.
    * If the array is empty place the value -1 in register 3.
    * Otherwise copy the last element of the array into register 3.
    */
  lazy val lastElement: MachineCode = {
    val code: Code =
      ifStmt(ADD(Reg.result, Reg.input2), eqCmp, const(0),
        const(-1),
        deref(elementAddr(binOp(ADD(Reg.result, Reg.input2), minus, const(1))))
      )
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit two's-complement integers.
    * Register 2 holds the number of elements in the array.
    * Determine the maximum of all the elements of the array, write it into register 3.
    * Assume the array is not empty.
    */
  lazy val arrayMaximum: MachineCode = {
    
    
    
    
    
    
    val i = new Variable("i")
    val maximum = new Variable("maximum")
    def element = deref(elementAddr(read(Reg.result, i)))
    val code: Code =
      Scope(Seq(i, maximum), block(
        assign(maximum, deref(ADD(Reg.result, Reg.input1))),
        assign(i, const(1)),
        whileLoop(read(Reg.result, i), ltCmp, ADD(Reg.result, Reg.input2), block(
          ifStmt(element, gtCmp, read(Reg.result, maximum),
            assign(maximum, element)
          ),
          assign(i, binOp(read(Reg.result, i), plus, const(1)))
        )),
        read(Reg.result, maximum)
      ))
    compilerA4(code)
  }

  /** Register 1 holds the address of the beginning of an array of 32-bit integers, each representing a character.
    * The integer zero represents a space, and each integer i (1 <= i <= 26) represents the i'th letter of the
    * uppercase alphabet.
    * Register 2 holds the number of elements in the array (can be empty).
    * Your program should output the uppercase characters represented by the integers in the array.
    * The MIPS system allows you to output one character at a time, by storing its ASCII value into the
    * special memory location CPU.printAddr (1111 1111 1111 1111 0000 0000 0000 1100).
    *
    * Hint: use Google to find an ASCII code table.
    */
  
  lazy val outputLetters: MachineCode = {
    val i = new Variable("i")
    def element = deref(elementAddr(read(Reg.result, i)))
    val code: Code =
      Scope(Seq(i), block(
        assign(i, const(0)),
        whileLoop(read(Reg.result, i), ltCmp, ADD(Reg.result, Reg.input2), block(
          ifStmt(element, eqCmp, const(0),
            printChar(const(32)), // ' '
            printChar(binOp(element, plus, const(64))) // 1 -> 'A' (65)
          ),
          assign(i, binOp(read(Reg.result, i), plus, const(1)))
        ))
      ))
    compilerA4(code)
  }

  /** Register 1 holds a 32-bit integer (in two's-complement notation).
    * Your program should format this integer in base 10, print it, then print a newline character.
    */

  /* Strategy: if n < 0, print '-' and negate n. The magnitude is then treated as unsigned, so that
   * -2^31 (whose negation is itself as a bit pattern) is still read correctly as 2^31.
   * Find the largest power of ten `p` with n / p <= 9, then print (n / p) % 10 for p = p, p/10, ..., 1.
   */
  lazy val printIntegerCode: Code = {
    val n = new Variable("n")
    val p = new Variable("p")
    Scope(Seq(n, p), block(
      write(n, Reg.input1),
      ifStmt(read(Reg.result, n), ltCmp, const(0), block(
        printChar(const(45)), // '-'
        assign(n, binOp(const(0), minus, read(Reg.result, n)))
      )),
      assign(p, const(1)),
      whileLoop(binOp(read(Reg.result, n), divideUnsigned, read(Reg.result, p)), gtUnsignedCmp, const(9),
        assign(p, binOp(read(Reg.result, p), times, const(10)))
      ),
      whileLoop(read(Reg.result, p), neCmp, const(0), block(
        printChar(binOp(
          binOp(binOp(read(Reg.result, n), divideUnsigned, read(Reg.result, p)), remainderUnsigned, const(10)),
          plus, const(48))), // '0'
        assign(p, binOp(read(Reg.result, p), divideUnsigned, const(10)))
      )),
      printChar(const(10)) // '\n'
    ))
  }
  lazy val printInteger: MachineCode = compilerA4(printIntegerCode)
}
