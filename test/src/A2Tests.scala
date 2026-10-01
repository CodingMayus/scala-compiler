import cs241e.assignments.{A1, Assembler}
import cs241e.assignments.A2.*;
import cs241e.mips.State.*
import cs241e.mips.{Bits, Word}
import org.scalatest.funsuite.AnyFunSuite
import cs241e.assignments.ProgramRepresentation;
import cs241e.mips.*;
import cs241e.assignments.Assembler.*;
/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

//class A2Tests extends AnyFunSuite {
//  val foo1 = Seq[Word](
//    LIS(Reg(1)),
//    Word(encodeSigned(-2)),
//    LIS(Reg(2)),
//    Word(encodeSigned(2)),
//    JR(Reg(31))
//  );
//
//  test("print foo 1"){
//    println(fool1)
//    codePrinter.pprintln(foo1)
//    disassemblingCodePrinter.pprintln(foo1);
//  }
//  val foo2 = block(
//    LIS(Reg(1)),
//    Word(encodeSigned(-2)),
//    LIS(Reg(2)),
//    Word(encodeSigned(2)),
//    JR(Reg(31))
//  )
//  test("print foo 2"){
//      dissasemblingCodePrinter.pprintln(foo2);
//    disassemblingCodePrinter.pprintln(eliminateBlocks(foo2));
//  }
//  val label1 = Label("first label")
//  // now you defined object you can use the code.
//  val foo3 = block(
//    LIS(Reg(1)),
//    Comment("This is a comment."),
//    block(
//    Word(encodeSigned(-2)),
//      USE(label1),
//      DEFINE(label1)
//    LIS(Reg(2)),
//    Word(encodeSigned(2))),
//    JR(Reg(31))
//  )
//  test("print foo 3") {
//    dissasemblingCodePrinter.pprintln(foo3);
//    disassemblingCodePrinter.pprintln(eliminateBlocks(foo3));
//    dissassemblingCodePrinter.pprintln(eliminateComments(foo3));
//    disassemblingCodePrinter.pprintln(eliminateComments(eliminateBlocks(foo3)));
//  }
//  val bar1 = block(
//    LIS(reg(2)),
//    Word(encodeSigned(2)),
//    LIS(Reg(1)),
//    Word(encodeSigned(-3)),
//    Comment("Now adding the numbers: "),
//    ADD(Reg(3), Reg(1),Reg(2)),
//    JR(Reg(31))
//  )
//
//
//  test("print bar 1"){
//    val machineCode = eliminateLabels(bar1);
//    val initState = A1.setMem(eliminateLabels(bar1));
//    val endState = CPU.run(initalState);
//    println(initState);
//    println(endState);
//    // to show the labels and the comments before a given memory address.
//    // create breakpoints so code knows to stop there.
//    // must keep blocks, labels, and comments.
//    val debugTable = createDebugTable();
//    codePrinter.pprintln(debugTable);
//    val endState2 = Debugger.debug(initalState, debugTable);
//    printn(endState2);
//  }

//}
