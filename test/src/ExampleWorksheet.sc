import cs241e.assignments.Assembler
import cs241e.assignments.*
import cs241e.mips.{State, Word}
// This is an example of a worksheet. Code that you write here will be immediately
// executed and you will be shown the results. You can use this for quickly testing
// your code. For example:

// The following will print 2:
Assembler.decodeSigned(Seq(false,false))
// The following will give a NotImplementedError until you implement the
// decodeUnsigned method, which is the first thing to do for Assignment 1.
Assembler.decodeUnsigned(Seq(false))

Assembler.encodeSigned(-4,3)
val test = Reg(5);


val ok = Assembler.encodeSigned(test.number)

//println(ok)



var test2 = State()

test2.setReg(1,Word(Assembler.encodeUnsigned(5)));
println(test2.reg(1))

println(A1.setMem(A1.maximum))


//A1.loadAndRun(A1.addOne,Word(Assembler.encodeSigned(1)),Word(Assembler.encodeSigned(2)));
A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(3)), Word(Assembler.encodeSigned(4))).reg(3)

