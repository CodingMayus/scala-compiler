
import cs241e.assignments.MemoryManagement.*;
import cs241e.assignments.Transformations.*;
import cs241e.assignments.Transformations.*;
import cs241e.assignments.ProgramRepresentation.*;
import cs241e.mips.*;
import cs241e.assignments.*;
import cs241e.assignments.Assembler.*;
import org.scalatest.funsuite.AnyFunSuite;

class A3Tests extends AnyFunSuite {
  val var1 = new Variable("foo");
  val var2 = new Variable("bar");
  val code = block(
    Comment("Hello there, can someone please ask a question?"),
    LIS(Reg(7)),
    Word(encodeUnsigned(42)),
    write(var1, Reg(7)),
    read(Reg(8), var1),
    write(var2, Reg(7)),
    read(Reg(8), var2),
    JR(Reg(31))
  )
  println(code)
  val eliminated: Code = eliminateVarAccessesA3(code, Chunk(Seq(var1, var2)))
  val compiled = compilerA3(code, Seq(var1, var2));
  //println(eliminated)
 // val compiled = compilerA3(code, Seq(var1, var2))
  test("print"){
   /// disassemblingCodePrinter.pprintln(code)
    //disassemblingCodePrinter.pprintln(eliminated)
    //disassemblingCodePrinter.pprintln(compiled)
    val initialState = A1.setMem(compiled.words);
    val finalState = CPU.run(initialState);
    println(finalState)
    println("size: "+finalState.mem(finalState.reg(29)))
    print("variable: "+finalState.mem(Word(encodeUnsigned(decodeUnsigned(finalState.reg(29))+8))))
  };
}
