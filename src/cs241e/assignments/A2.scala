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

import Assembler.*
import ProgramRepresentation.*
import Transformations.*
import cs241e.mips.*

object A2 {
  /* As part of Assignment 2, before implementing the methods in this file, first implement the methods in
   * Transformations.scala in the section for Assignment 2.
   */

  /* Registers 1 and 2 hold 32-bit integers (in two's-complement notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  // Determines if A < B, then if true, go to BGreaterA label, which sets register 3 to B then jumps to register 31 to 
  //                                           to end excecution.
  // Using BNE
  //                           if false, sets register 3 to A, then jumps to register 31 to end execution
  lazy val maximum: Seq[Word] = {
    val RegOneGreaterOrEqualSigned = new Label("RegOneGreaterOrEqualSigned");
    // left side is the implementation program's label naming
    // right side is the programming language.
    val RegTwoGreaterSigned = new Label("RegTwoGreaterSigned")
    val code = Seq[Code](
      CodeWord(SLT(Reg(3), Reg(1), Reg(2))),
      beq(Reg(0), Reg(3), RegOneGreaterOrEqualSigned),
      Define(RegTwoGreaterSigned),
      CodeWord(ADD(Reg(3), Reg(2),Reg(0))),
      CodeWord(JR(Reg(31))),
      Define(RegOneGreaterOrEqualSigned),
      CodeWord(ADD(Reg(3), Reg(1), Reg(0))),
      CodeWord(JR(Reg(31)))
    )
    eliminateLabels(code)
  }

  /* Registers 1 and 2 hold 32-bit integers (in unsigned integer notation). Place the maximum of these integers
   * in register 3, and end execution.
   */
  // Determines if A < B, then if true, go to BGreaterA label, which sets register 3 to B then jumps to register 31 to 
  //                                           to end excecution.
  // Using BNU
  //                           if false, sets register 3 to A, then jumps to register 31 to end execution
  val RegOneGreaterOrEqualUnsigned = new Label("RegOneGreaterOrEqualUnsigned");
  val RegTwoGreaterUnsigned=new Label("RegTwoGreaterUnsigned");
  lazy val maximumUnsigned: Seq[Word] = {
    val code = Seq[Code](
      CodeWord(SLTU(Reg(3), Reg(1), Reg(2))),
      beq(Reg(0),Reg(3), RegOneGreaterOrEqualUnsigned),
      Define(RegTwoGreaterUnsigned),
      CodeWord(ADD(Reg(3), Reg(0), Reg(2))),
      CodeWord(JR(Reg(31))),
        Define(RegOneGreaterOrEqualUnsigned),
      CodeWord((ADD(Reg(3), Reg(0), Reg(1)))),
      CodeWord(JR(Reg(31)))
    )
    eliminateLabels(code)
  }

}

