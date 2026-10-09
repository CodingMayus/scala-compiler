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

import ProgramRepresentation.*
import Assembler.*
import cs241e.mips.*

/** Methods that generate `Code` for various higher-level language features. */

object CodeBuilders { 
  /* ## Assignment 4 */

  /* Complete the implementation of the following methods by replacing the `???`. */

  /** Generates a binary operation that evaluates `e1` and `e2`, ensures that the `Reg.result` from
    * `e1` is in `Reg.scratch` and that the `Reg.result` from `e2` is still in `Reg.result`,
    * then executes `op`.
    *
    * Hint: Use a temporary variable and a `Scope`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def binOp(e1: Code, op: Code, e2: Code): Code = {
    val tempVar = new Variable("tempVar");

      Scope(Seq(tempVar),
        block(
          e1,
          write(tempVar,Reg.scratch),
          e2,
          read(Reg.result, tempVar),
          op
        )
      )
  }

  /* The following `Code`s are intended to be used as the `op` argument to `binOp`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, compute the corresponding arithmetic
   * operation, and leave the result of the operation in `Reg.result`.
   *
   * Assume that the operands are to be interpreted as signed two's-complement integers except in
   * `divideUnsigned` and `remainderUnsigned`.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  lazy val plus: Code = ADD(Reg.result, Reg.result, Reg.scratch)
  lazy val minus: Code = SUB(Reg.result, Reg.result, Reg.scratch)
  lazy val times: Code = block(MULT( Reg.result, Reg.scratch), MFHI(Reg.result), MFLO(Reg.result))
  lazy val divide: Code = block(DIV(Reg.result, Reg.scratch), MFHI(Reg.result))
  // is this the correct behaviour?
  lazy val remainder: Code = block(DIV(Reg.result, Reg.scratch), MFLO(Reg.result))
  lazy val divideUnsigned: Code = block(DIVU(Reg.result, Reg.scratch), MFHI(Reg.result))
  lazy val remainderUnsigned: Code = block(DIV(Reg.result, Reg.scratch), MFLO(Reg.result))

  /* The following `Code`s are intended to be used as the `comp` argument to `IfStmt`.
   * They should expect two operands in `Reg.scratch` and `Reg.result`, interpret them as two's-complement
   * signed integers (except `gtUnsignedCmp`), compare them, and branch to `label` if the comparison fails.
   *
   * The generated code may modify the values of Reg.result and Reg.scratch. If you
   * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
   * must not modify the values of any other registers that are already listed in Reg.scala.
   */
  def eqCmp(label: Label): Code = (bne(Reg.scratch, Reg.result, label))
  def neCmp(label: Label): Code = (beq(Reg.scratch, Reg.result, label))
  def ltCmp(label: Label): Code = block(SLT(Reg.result,Reg.result, Reg.scratch),
    bne(Reg.result, Reg.zero, label)
  )
  def gtCmp(label: Label): Code = block(SLT(Reg.result, Reg.scratch, Reg.result),
    bne(Reg.result, Reg.zero, label))
  def leCmp(label: Label): Code = block(SLT(Reg.result, Reg.scratch, Reg.result),
    beq(Reg.result, Reg.zero, label))
  def geCmp(label: Label): Code = block(SLT(Reg.result,Reg.result, Reg.scratch),
    beq(Reg.result, Reg.zero, label))
  def gtUnsignedCmp(label: Label): Code = block(SLTU(Reg.result, Reg.scratch, Reg.result),
    bne(Reg.result, Reg.zero, label))

  /** Generates code that evaluates `expr` to yield a memory address, then loads the word from that address
    * into `Reg.result`.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    **/

  // assumes after evaluating the Code, expr, it loads the result into Reg.result
  def deref(expr: Code): Code = (
              block(
                expr,
                LW(Reg.result,0, Reg.result)
              )

  )

  /** Generates code that evaluates `target` to yield a memory address, then evaluates `expr` to yield a value,
    * then stores the value into the memory address.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def assignToAddr(target: Code, expr: Code): Code = {

    val tempVar= new Variable("tempVar");
    Scope( Seq(tempVar),
    block(
      target,
      write(tempVar,Reg.result),
      expr,
      read(Reg.scratch, tempVar),
      SW(Reg.result, 0, Reg.scratch)
    ))
  }

  /** Generates code that implements a while loop. The generated code should evaluate `e1` and `e2`,
    * compare them using `comp`, and if the comparison succeeds, it should execute `body` and repeat
    * the entire process from the beginning.
    *
    * The generated code may modify the values of Reg.result and Reg.scratch. If you
    * need more than these registers, you may add new scratch registers to Reg.scala. The generated code
    * must not modify the values of any other registers that are already listed in Reg.scala.
    */
  def whileLoop(e1: Code, comp: Label=>Code, e2: Code, body: Code): Code = {
    val loopStart:Label = new Label("loopStart");
    val expressionOne= new Variable("expressionOne")
    // how does this compiler handle tempVariable repeats ?? 
    val continue:Label = new Label("continue");
    val loopEnd: Label = new Label("loopEnd")
    Scope(
    Seq(expressionOne),
      block(
      Define(loopStart),
        e1,
        write(expressionOne,Reg.result),
        e2,
        read(Reg.scratch,expressionOne),
      comp(loopEnd),
        body,
        LIS(Reg.scratch),
        Use(loopStart),
        JR(Reg.scratch),
        Define(loopEnd)
    )
    )
  }

  
}
