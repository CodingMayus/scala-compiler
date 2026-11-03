import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.Chunk
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.{CPU, Word}
import org.scalatest.funsuite.AnyFunSuite

class A7aTests extends AnyFunSuite {
  test("recognize") {
    assert(Scanning.recognize(A7.decimalNumber, "-128".toList) == true)

    assert(Scanning.recognize(A7.notDiv23, "10001".toList) == true)
    assert(Scanning.recognize(A7.notDiv23, "1011".toList) == true)
    assert(Scanning.recognize(A7.notDiv3,"10101".toList)==false)
    assert(Scanning.recognize(A7.notDiv3, "11".toList) == false)
    assert(Scanning.recognize(A7.notDiv3, "100".toList) == true)
    assert(Scanning.recognize(A7.notDiv23, "101".toList)==true)
    assert(Scanning.recognize(A7.notDiv23,"100110101".toList)==false);
    assert(Scanning.recognize(A7.notDiv23, "100110101".toList) == false);
    assert(Scanning.recognize(A7.notDiv23, "1001".toList)==false);
    assert(Scanning.recognize(A7.notDiv23, "8".toList)==false)
    assert(Scanning.recognize(A7.decimalNumber, "-128".toList)==true)
    assert(Scanning.recognize(A7.decimalNumber, "127".toList) == true)
    assert(Scanning.recognize(A7.decimalNumber, "127-".toList) == false)
    assert(Scanning.recognize(A7.decimalNumber, "1283232".toList) == false)
    assert(Scanning.recognize(A7.decimalNumber, "50".toList) == true)
    assert(Scanning.recognize(A7.decimalNumber, "-60".toList) == true)
    assert(Scanning.recognize(A7.decimalNumber, "18".toList) == true)
    assert(Scanning.recognize(A7.decimalNumber, "108".toList) == true)

    assert(Scanning.recognize(A7.decimalNumber, "-18".toList) == true)

    assert(Scanning.recognize(A7.abc, "abc".toList) == true)
    assert(Scanning.recognize(A7.abc, "ab".toList) == false)
    assert(Scanning.recognize(A7.abc, "a".toList) == false)
    assert(Scanning.recognize(A7.abc, "abbbbbabc".toList) == true)
    assert(Scanning.recognize(A7.abc, "cbacccca".toList) == false)
    assert(Scanning.recognize(A7.abc, "abbbbbcbc".toList) == true)
    assert(Scanning.recognize(A7.abcSubstring, "abbbbbcbc".toList) == false)
    assert(Scanning.recognize(A7.abcSubstring, "abc".toList) == true)
    assert(Scanning.recognize(A7.abcSubstring, "aaaaaaaabc".toList) == true)
    assert(Scanning.recognize(A7.abcSubstring, "aaaaaaaab".toList) == false)
    assert(Scanning.recognize(A7.abcSubstring, "aaaaaaaadbc".toList) == false)
    assert(Scanning.recognize(A7.abcSubstring, "aaaaaaaaabc".toList) == true)

  }
}