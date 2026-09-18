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
/* ## Assignment 1 */

package cs241e.assignments

import cs241e.assignments.Assembler.*
import cs241e.mips.*

object A1 {
  /* As part of Assignment 1, before implementing the methods in this file, first implement the first four methods
   * in Assembler.scala.
   */

  /** The `setMem` method in `State` loads one word into a specific memory location identified by
    * a given address. Implement this extended `setMem` method that takes a sequence of `words` and writes them
    * into successive memory locations in `inputState` starting with a given `startingAddress`. Return the
    * resulting state.
    */
  def setMem(words: Seq[Word], inputState: State = State(), startingAddress: Word = Word.zero): State = {
    require(decodeUnsigned(startingAddress) + words.size*4 <= decodeUnsigned(CPU.maxAddr))
    var nextAddress = encodeUnsigned(decodeUnsigned(startingAddress))
    var newState = inputState;
    // since inputState is immutable ( by default D: )
    for  i:Int <- words.indices do {
//      print(words(i));
//      print(" "+nextAddress)
//      println()
//
      newState = newState.setMem(Word(nextAddress), words(i));
     // println(newState)
      nextAddress = encodeUnsigned(decodeUnsigned(nextAddress) + 4)
    }

    return newState;
  }

  /** You can use this helper method to test the following programs that you will write. It loads the
    * given `words` into memory, writes the specified values to registers 1 and 2, and then runs
    * the CPU on the `words` that were loaded.
    */
  def loadAndRun(words: Seq[Word], register1: Word = Word.zero, register2: Word = Word.zero): State = {
    val initialState = {
      setMem(words)
        .setReg(1, register1)
        .setReg(2, register2)
    }
    CPU.run(initialState)
  }

  /** Write a MIPS machine language program that transfers control to the address saved in register 31. This is the only
    * thing that your program should do.
    *
    * Hint: You can create a `Word` of 32 bits as follows: `Word("01010101010101010101010101010101")`.
    */
  lazy val transferTo31 = Seq[Word](Word("00000011111000000000000000001000"))

  /** Write a MIPS machine language program that copies the value in register 1 to register 3, then adds the values
    * in register 1 and 3, placing the result in register 4, and then ends execution by transferring control to the
    * address in register 31.
    */
  // 1: add instruction with register 0 and register 1 being the source, and register 3 being the destination
  // 2: add instruction iwth register 1 and register 3 being the source, and register 4 being the destination
  // 3: jumps to register 31 (transferTo31)
  lazy val add134 = Seq[Word](Word("00000000000000010001100000100000"),Word("00000000001000110010000000100000"),
    transferTo31.head);

  /* Now implement the code generation methods in the second half of `Assembler.scala`. Then continue here
   * with the following methods.
   */

  /** Write a MIPS machine language program that determines the maximum of the values in registers 1 and 2
    * interpreted as two's-complement integers, places it in register 3, and ends execution.
   * ( does ending the execution here mean jumping to register 31? probably ).
    */
  // 1. signed set less than, instruction that determines which is bigger, registers  and registers 2, and saves in regsiter3.
  // If the number in register 3 is:
  // 1 -> copy $t into register 3
  // 0 -> copy $s into register 3
  lazy val maximum = Seq[Word](SLT(Reg(3),Reg(1),Reg(2)), BNE(Reg(3), Reg(0), 2),ADD( Reg(3), Reg(1), Reg(0)), transferTo31.head, ADD( Reg(3), Reg(2), Reg(0)),transferTo31.head);

  lazy val maximumPart1 =  Seq[Word](SLT(Reg(3), Reg(1),Reg(2)), transferTo31.head)
  /** Write a MIPS machine language program that adds 1 to the value in register 1, places the result in register 3,
    * and then ends execution.
    */
  //
  // the order of the elements is different from the mips reference..... and machine code order...
  lazy val addOne = Seq[Word](LIS(Reg(2)), Word(encodeUnsigned(1)),ADD(Reg(3), Reg(2), Reg(1)),transferTo31.head)

  /** Write a MIPS machine language program that interprets the value in register 1 as the address of a word
    * in memory, places the address of the following word in memory in register 3, and then ends execution.
     */
  lazy val followingAddress = Seq[Word](LIS(Reg(2)), Word(encodeUnsigned(4)), ADD(Reg(3), Reg(2), Reg(1)), transferTo31.head)
}

