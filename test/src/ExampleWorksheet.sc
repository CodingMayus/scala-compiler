import cs241e.assignments.A2.{RegOneGreaterOrEqualUnsigned, RegTwoGreaterUnsigned}
import cs241e.assignments.Assembler.*
import cs241e.assignments.A2.maximumUnsigned;
import cs241e.assignments.A2.maximum;
import cs241e.assignments.A2;

import cs241e.assignments.ProgramRepresentation.{Code, CodeWord, Comment, Define, Label, Use, beq, block, codePrinter, disassemblingCodePrinter}
import cs241e.assignments.{A1, Assembler, Reg}
import cs241e.mips.*
import cs241e.assignments.Transformations.{eliminateBlocks, eliminateComments, eliminateLabels};
//import cs241e.assignments.Assembler
//import cs241e.assignments.*
//import cs241e.mips.{State, Word}
//// This is an example of a worksheet. Code that you write here will be immediately
//// executed and you will be shown the results. You can use this for quickly testing
//// your code. For example:
//
//// The following will print 2:
//Assembler.decodeSigned(Seq(false,false))
//// The following will give a NotImplementedError until you implement the
//// decodeUnsigned method, which is the first thing to do for Assignment 1.
//Assembler.decodeUnsigned(Seq(false))
//
//Assembler.encodeSigned(-4,3)
//val test = Reg(5);
//
//
//val ok = Assembler.encodeSigned(test.number)
//
////println(ok)
//
//
//
//var test2 = State()
//
//test2.setReg(1,Word(Assembler.encodeUnsigned(5)));
//println(test2.reg(1))
//
//println(A1.setMem(A1.maximum))
//
//
////A1.loadAndRun(A1.addOne,Word(Assembler.encodeSigned(1)),Word(Assembler.encodeSigned(2)));
////A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(3)), Word(Assembler.encodeSigned(4))).reg(3)
//


val code = Seq[Code](
  CodeWord(SLTU(Reg(3), Reg(1), Reg(2))),
  beq(Reg(0), Reg(3), RegOneGreaterOrEqualUnsigned),
  Define(RegTwoGreaterUnsigned),
  CodeWord(ADD(Reg(3), Reg(0), Reg(2))),
  CodeWord(JR(Reg(31))),
    Define(RegOneGreaterOrEqualUnsigned),
  CodeWord((ADD(Reg(3), Reg(0), Reg(1)))),
  CodeWord(JR(Reg(31)))

)
disassemblingCodePrinter.pprintln(eliminateLabels(code));
codePrinter.pprintln(eliminateLabels(code))
//println(eliminateLabels(code))



val code2 = Seq[Code](
  CodeWord(SLTU(Reg(3), Reg(1), Reg(2))),
  beq(Reg(0), Reg(3), RegOneGreaterOrEqualUnsigned),
  Define(RegTwoGreaterUnsigned),
  Comment("Cheese Borger"),
  CodeWord(ADD(Reg(3), Reg(0), Reg(2))),
  CodeWord(JR(Reg(31))),
  Comment("LALALALAA"),
  Define(RegOneGreaterOrEqualUnsigned),
  CodeWord((ADD(Reg(3), Reg(0), Reg(1)))),
  CodeWord(JR(Reg(31)))
)


println("hi")

val label1 = new Label("label1");

val endState = A1.loadAndRun(maximumUnsigned, Word(Assembler.encodeUnsigned(6)),Word(Assembler.encodeUnsigned(8)));
println(endState.reg(3));
assert(A1.loadAndRun(maximumUnsigned, Word(Assembler.encodeUnsigned(4294967295L)),Word(Assembler.encodeUnsigned(1))).reg(3)== Word(Assembler.encodeUnsigned(4294967295L)));
println(A1.loadAndRun(A2.maximum, Word(Assembler.encodeSigned(6000)),Word(Assembler.encodeSigned(700))).reg(3))
println(A1.loadAndRun(A2.maximum, Word(Assembler.encodeSigned(-2147483648L)),Word(Assembler.encodeSigned(1))).reg(3));
