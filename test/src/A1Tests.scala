import cs241e.assignments.{A1, Assembler}
import cs241e.mips.State.*
import org.scalatest.funsuite.AnyFunSuite
import cs241e.mips.{Bits, Word}

/* This is an example of a class for running and testing your code. Add other testing methods here
 * and/or create additional classes like this one.
 *
 * Run the tests by right-clicking on the test code and selecting Run '(test name)' from the menu.
 *
 * You can also run the tests by right-clicking on a file or directory containing tests in the Project navigator
 * on the left.
 */

class A1Tests extends AnyFunSuite {
  test("decodeUnsigned") {

    println("You can print output from your tests.")


    assert(1 + 1 == 2, "1 + 1 did not equal 2.")
    // The following will fail until you implement decodeUnsigned as part of Assignment 1.
    println(Assembler.decodeUnsigned(Seq(false)))
    assert(Assembler.decodeUnsigned(Seq(true))==1)
    assert(Assembler.decodeUnsigned(Seq(true,true))==3)
    assert(Assembler.decodeUnsigned(Seq(true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true,
      true))==4294967295L)

    assert(Assembler.decodeSigned(Seq(true,true))== -1)
    assert(Assembler.decodeSigned(Seq(false, false)) ==0 )
    assert(Assembler.decodeSigned(Seq(true))== -1)

    assert(Assembler.decodeSigned(Seq(false,false,false,false,false,false,true))==1)
    assert(Assembler.decodeUnsigned(Seq(false,false,false,false,false,false,true))==1)
  for ( i <- -4 to 3){
    assert(Assembler.decodeSigned(Assembler.encodeSigned(i,3))==i)
  }
    for ( i <- -4 to 3){
      assert(Assembler.decodeSigned(Assembler.encodeSigned(i))==i)
    }
    for( i <- -29309 to 9032903){
      assert(Assembler.decodeSigned(Assembler.encodeSigned(i))==i)
    }


    assert(Assembler.decodeUnsigned(Seq(false)) == 0)
    assert(Assembler.decodeUnsigned(Seq(true)) == 1)
    assert(Assembler.decodeUnsigned(Seq(true, true)) == 3)
    assert(Assembler.decodeUnsigned(Seq(true, false)) == 2)
    assert(Assembler.decodeUnsigned(Seq(false, true)) == 1)
    assert(Assembler.decodeSigned(Seq(false, true)) == 1)
    assert(Assembler.decodeSigned(Seq(false, false)) == 0)
    assert(Assembler.decodeSigned(Seq(true, true)) == -1)
    assert(Assembler.decodeSigned(Seq(true)) == -1)
    assert(Assembler.decodeSigned(List(true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true)) == -1)

//    for (i <- -4 to 3) {
//      assert(Assembler.decodeSigned(Assembler.encodeSigned(i, 3)) == i)
//    }
    for (i <- 0 to 7) {
      assert(Assembler.decodeUnsigned(Assembler.encodeUnsigned(i, 3)) == i)
    }
  }
  test("Functions"){
val initalState = A1.loadAndRun(A1.maximum,Word(Assembler.encodeSigned(6)),Word(Assembler.encodeSigned(8)));
//println(initalState)
val test = A1.loadAndRun(A1.add134, Word(Bits("01"*16)), Word(Bits("0"*31+"1")))
    test.setReg(3, Word(Assembler.encodeUnsigned(32903923)))

    //A1.loadAndRun(A1.transferTo31).reg(1)
 assert(Assembler.decodeUnsigned(initalState.reg(3))==8)
    assert(A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(3)), Word(Assembler.encodeSigned(4))).reg(3) == Word(Assembler.encodeSigned(4)))

    assert(A1.loadAndRun(A1.addOne, Word(Assembler.encodeSigned(20)),Word.zero).reg(3)==Word(Assembler.encodeUnsigned(21)))
    assert(A1.loadAndRun(A1.addOne, Word(Assembler.encodeUnsigned(4294967294L)), Word.zero).reg(3)==Assembler.encodeUnsigned(4294967295L))
    assert(A1.loadAndRun(A1.addOne, Word(Assembler.encodeUnsigned(4294967295L)), Word.zero).reg(3) == Assembler.encodeUnsigned(0))
    assert(A1.loadAndRun(A1.addOne, Word(Assembler.encodeSigned(2147483647)), Word.zero).reg(3) == Assembler.encodeSigned(-2147483648))
    println(A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(-5)), Word(Assembler.encodeSigned(-1))))
    println(A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(6)), Word(Assembler.encodeSigned(5))))

    //assert(A1.loadAndRun(A1.maximum, Word(Bits("11111111111111111111111111111101")), Word(Bits("11111111111111111111111111111011"))).reg(3)==Word(Bits("11111111111111111111111111111101")))
    assert(A1.loadAndRun(A1.maximum, Word(Assembler.encodeSigned(-1)), Word(Assembler.encodeSigned(-5))).reg(3) == Word(Assembler.encodeSigned(-1)))
    
    assert(A1.loadAndRun(A1.followingAddress, Word(Assembler.encodeUnsigned(4)),Word.zero).reg(3)==Assembler.encodeUnsigned(8))

  }
}
