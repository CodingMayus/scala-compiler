import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.Chunk
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite

class A4Tests extends AnyFunSuite {
  test("scala") {
    def proc() = {
      var v = 5
      if(1+1 == 2) v = 2*(3+v) else v = 42
      v
    }
    println(proc())
  }
  test("lacs") {
    val v = new Variable("v")
    val code = Scope(Seq(v),
      block(
        assign(v, const(5)),
        ifStmt(binOp(const(1), plus, const(1)), eqCmp, const(2),
          assign(v, binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))),
          assign(v, const(42))
        ),
        read(Reg.result, v)
      ))
    val machineCode = compilerA4(code)
    //disassemblingCodePrinter.pprintln(machineCode)
    val finalState = A4.loadAndRun(machineCode)
    //println(decodeSigned(finalState.reg(3)))
  }

  def const(i: Int) = {
    block(LIS(Reg.result), Word(encodeSigned(i)))
  }
  test("arrayScala") {
    val array = Array(2,4,6,8)
    println(array(2))
  }
  test("arrayLacs") {
    val array = Seq[Word](
      Word(encodeSigned(2)),
      Word(encodeSigned(4)),
      Word(encodeSigned(6)),
      Word(encodeSigned(8)),
    )
    val arrayAddr = new Variable("arrayAddr")
    val size = new Variable("size")
    val code = Scope(Seq(arrayAddr, size), block(
      write(arrayAddr, Reg(1)),
      write(size, Reg(2)),

      // *(arrayAddr + 2*4)
      deref(binOp(read(Reg.result, arrayAddr), plus,
        binOp(const(2), times, const(4))
      ))
    ))
    val machineCode = compilerA4(code)
    //    disassemblingCodePrinter.pprintln(machineCode)
    val finalState = A4.loadAndRunArray(machineCode, array)
    println(decodeSigned(finalState.reg(3)))
  }

  // Interactive Debugger!
  test("debugger") {
    val v = new Variable("v")
    val code = Scope(Seq(v), block(
      assign(v, const(5)),
      ifStmt(binOp(const(1), plus, const(1)), eqCmp, const(2),
        assign(v, binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))),
        assign(v, const(42))
      ),
      Comment("BREAK - before read"),
      read(Reg.result, v),
      Comment("BREAK - after read")
    ))
    val machineCode = compilerA4(code)
    disassemblingCodePrinter.pprintln(machineCode)
    val debuggerObject = A4.interactiveDebugger(machineCode)
    debuggerObject.regs
    debuggerObject.run
    debuggerObject.step
    debuggerObject.mem("00000000111111111111111111100100")
    debuggerObject.run
    //debuggerObject.regs
  }

  def words(xs: Int*): Seq[Word] = xs.map(x => Word(encodeSigned(x)))
  def run(code: Code): Long = decodeSigned(A4.loadAndRun(compilerA4(code)).reg(3))

  test("lacs result") {
    val v = new Variable("v")
    val code = Scope(Seq(v),
      block(
        assign(v, const(5)),
        ifStmt(binOp(const(1), plus, const(1)), eqCmp, const(2),
          assign(v, binOp(const(2), times, binOp(const(3), plus, read(Reg.result, v)))),
          assign(v, const(42))
        ),
        read(Reg.result, v)
      ))
    assert(run(code) == 16)
  }

  test("arithmetic ops") {
    assert(run(binOp(const(7), plus, const(-3))) == 4)
    assert(run(binOp(const(7), minus, const(3))) == 4)
    assert(run(binOp(const(-7), times, const(3))) == -21)
    assert(run(binOp(const(17), divide, const(5))) == 3)
    assert(run(binOp(const(-17), divide, const(5))) == -3)
    assert(run(binOp(const(17), remainder, const(5))) == 2)
    assert(run(binOp(const(-1), divideUnsigned, const(2))) == Int.MaxValue)
    assert(run(binOp(const(-1), remainderUnsigned, const(10))) == 5) // 4294967295 % 10
    assert(run(binOp(binOp(const(10), minus, const(4)), minus, binOp(const(1), plus, const(1)))) == 4)
  }

  test("comparisons") {
    val cmps = Seq[(Label => Code, (Int, Int) => Boolean)](
      (eqCmp, _ == _), (neCmp, _ != _), (ltCmp, _ < _), (gtCmp, _ > _), (leCmp, _ <= _), (geCmp, _ >= _),
      (gtUnsignedCmp, (a, b) => Integer.compareUnsigned(a, b) > 0)
    )
    val pairs = Seq((1, 2), (2, 1), (3, 3), (-1, 1), (1, -1), (-5, -5))
    for ((cmp, expected) <- cmps; (a, b) <- pairs) {
      val res = run(ifStmt(const(a), cmp, const(b), const(1), const(0)))
      assert(res == (if (expected(a, b)) 1 else 0), s"cmp failed for $a, $b")
    }
  }

  test("nested ifs and scopes") {
    val x = new Variable("x")
    val code = Scope(Seq(x), block(
      assign(x, const(0)),
      ifStmt(const(1), ltCmp, const(2),
        ifStmt(const(3), gtCmp, const(4), assign(x, const(1)), assign(x, const(2)))),
      read(Reg.result, x)
    ))
    assert(run(code) == 2)
  }

  test("deref and assignToAddr") {
    val code = block(
      assignToAddr(binOp(ADD(Reg.result, Reg(1)), plus, const(4)), const(99)),
      deref(binOp(ADD(Reg.result, Reg(1)), plus, const(4)))
    )
    val state = A4.loadAndRunArray(compilerA4(code), words(1, 2, 3))
    assert(decodeSigned(state.reg(3)) == 99)
  }

  test("whileLoop sum") {
    val i = new Variable("i")
    val sum = new Variable("sum")
    val code = Scope(Seq(i, sum), block(
      assign(i, const(0)), assign(sum, const(0)),
      whileLoop(read(Reg.result, i), leCmp, const(10), block(
        assign(sum, binOp(read(Reg.result, sum), plus, read(Reg.result, i))),
        assign(i, binOp(read(Reg.result, i), plus, const(1)))
      )),
      read(Reg.result, sum)
    ))
    assert(run(code) == 55)
  }

  test("lastElement") {
    assert(decodeSigned(A4.loadAndRunArray(A4.lastElement, Seq()).reg(3)) == -1)
    assert(decodeSigned(A4.loadAndRunArray(A4.lastElement, words(7)).reg(3)) == 7)
    assert(decodeSigned(A4.loadAndRunArray(A4.lastElement, words(2, 4, 6, -8)).reg(3)) == -8)
  }

  test("arrayMaximum") {
    def max(xs: Int*) = decodeSigned(A4.loadAndRunArray(A4.arrayMaximum, words(xs*)).reg(3))
    assert(max(5) == 5)
    assert(max(-5) == -5)
    assert(max(1, 9, 3) == 9)
    assert(max(9, 1, 3) == 9)
    assert(max(1, 3, 9) == 9)
    assert(max(-4, -2, -7) == -2)
    assert(max(Int.MinValue, Int.MaxValue, 0) == Int.MaxValue)
  }

  test("outputLetters") {
    def out(xs: Int*) = CPU.captureOutput(A4.loadAndRunArray(A4.outputLetters, words(xs*)))
    assert(out() == "")
    assert(out(8, 9, 0, 1, 26) == "HI AZ")
    assert(out(0) == " ")
  }

  test("printInteger") {
    def out(i: Int) = CPU.captureOutput(A4.loadAndRun(A4.printInteger, Word(encodeSigned(i))))
    for (i <- Seq(0, 1, -1, 7, 10, 42, -42, 100, 999, 1000, 123456789, -987654321,
                  1000000000, Int.MaxValue, Int.MinValue))
      assert(out(i) == s"$i\n", s"printInteger($i)")
  }
}
