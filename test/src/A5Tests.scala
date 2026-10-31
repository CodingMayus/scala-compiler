import cs241e.assignments.*
import cs241e.assignments.Assembler.*
import cs241e.assignments.CodeBuilders.*
import cs241e.assignments.MemoryManagement.*
import cs241e.assignments.ProgramRepresentation.*
import cs241e.assignments.Transformations.*
import cs241e.mips.*
import org.scalatest.funsuite.AnyFunSuite

class A5Tests extends AnyFunSuite {
  test("scala"){
      def main(a: Int, b:Int)={a+b}
      println(main(1,2))
  }
  test("lacs"){


    val a = new Variable("a")
    val b = new Variable("b")
    val main = new Procedure("main", Seq(a,b))
    main.code=(binOp(read(Reg.result,a),plus, read(Reg.result, b)))
    val machineCode = compilerA5(Seq(main))
    val endState = A4.loadAndRun(machineCode, register1 = Word(encodeSigned(1)), register2=Word(encodeSigned(2)), debug=false)
    println("urmom" +decodeSigned(endState.reg(3)))
    //disassemblingCodePrinter.pprintln(machineCode)

  }


  test("scala_fact"){
    def main(a:Int, b:Int)={fact(a)}
    def fact(i:Int):Int={
      if(i<=0)1 else i * fact(i-1)
    }
    println(main(10,2))
  }
  def const(i: Int)=block(
      LIS(Reg.result),
    Word(encodeSigned(i))

  )

  test("lacs_fact"){
    val a = new Variable("a");
    val b = new Variable("b");
    val main = new Procedure("main", Seq(a,b))
    val i = new Variable("i")
    val fact = new Procedure ("fact", Seq(i))
    main.code = call(fact, read(Reg.result, a))
    fact.code = ifStmt(read(Reg.result, i), leCmp, const(0), const(1),
      binOp(read(Reg.result,i), times, call(fact,binOp(read(Reg.result,i),minus, const(1)))))
    val machineCode = compilerA5(Seq(main, fact))
    val endState = A4.loadAndRun(machineCode, register1=Word(encodeSigned((10))), register2=Word(encodeSigned(3)), debug=false)
    println(decodeSigned(endState.reg(3)))
  }

  test("print Procedures"){
    // val main = new Procedure("main", Seq(const(10), const(2)))  // doesn't compile: params must be Variables, and `main` is redeclared below
    val main = A5.printProcedure;
    val machineCode = compilerA5(Seq(main))
    val endState = A4.loadAndRun(machineCode, register1=Word(encodeSigned(10)),register2=Word(encodeSigned(4)),debug=false)
    
  }

  // ---------- printing tests ----------
  private def wordsOf(xs: Int*): Seq[Word] = xs.map(x => Word(encodeSigned(x)))

  /** Compile printProcedure alone and run it with `i` in register 1 (its single argument). */
  private def printOut(i: Int): String =
    CPU.captureOutput(A4.loadAndRun(compilerA5(Seq(A5.printProcedure)), register1 = Word(encodeSigned(i))))

  /** Compile printArray (+ printProcedure) and run it on `xs`. */
  private def printArrayOut(xs: Int*): String =
    CPU.captureOutput(A4.loadAndRunArray(compilerA5(Seq(A5.printArray, A5.printProcedure)), wordsOf(xs*)))

  test("printProcedure prints single integers") {
    for (i <- Seq(0, 1, 7, 10, 42, -1, -42, 123456, -987654, 2147483647, -2147483648)) {
      assert(printOut(i) == s"$i\n", s"printProcedure($i)")
    }
  }

  test("printArray: empty array prints nothing") {
    assert(printArrayOut() == "")
  }

  test("printArray: one element") {
    assert(printArrayOut(5) == "5\n")
    assert(printArrayOut(-5) == "-5\n")
  }

  test("printArray: several elements") {
    assert(printArrayOut(1, 2, 3) == "1\n2\n3\n")
    assert(printArrayOut(10, -20, 0, 30) == "10\n-20\n0\n30\n")
  }

  test("printArray: elements are printed in order, not the indices") {
    // If this prints 0 1 2 the loop is printing i instead of array[i].
    assert(printArrayOut(100, 200, 300) == "100\n200\n300\n")
  }

  test("printArray: extreme values") {
    assert(printArrayOut(2147483647, -2147483648, 0) == "2147483647\n-2147483648\n0\n")
  }

  // ---------- treeHeight tests ----------

  /** A tree in Scala, used to build test arrays and compute the expected height. */
  case class T(value: Int, left: Option[T] = None, right: Option[T] = None)
  private def leaf(v: Int): T = T(v)
  private def node(v: Int, l: T, r: T): T = T(v, Some(l), Some(r))
  private def onlyLeft(v: Int, l: T): T = T(v, Some(l), None)
  private def onlyRight(v: Int, r: T): T = T(v, None, Some(r))

  /** Reference answer. */
  private def height(t: T): Int = 1 + math.max(t.left.map(height).getOrElse(0), t.right.map(height).getOrElse(0))

  /** Encode a tree in preorder: root at index 0, each node as (value, leftIndex, rightIndex). */
  private def encodePreorder(t: T): Seq[Int] = {
    val out = scala.collection.mutable.ArrayBuffer[Int]()
    def go(n: T): Int = {
      val at = out.size
      out ++= Seq(n.value, -1, -1)
      n.left.foreach(l => out(at + 1) = go(l))
      n.right.foreach(r => out(at + 2) = go(r))
      at
    }
    go(t)
    out.toSeq
  }

  /** Encode a tree with the root at index 0 but all other nodes placed in a shuffled order,
    * so children can come after their siblings' subtrees, right before left, etc. */
  private def encodeShuffled(t: T, seed: Long): Seq[Int] = {
    val nodes = scala.collection.mutable.ArrayBuffer[T]()
    def collect(n: T): Unit = { nodes += n; n.left.foreach(collect); n.right.foreach(collect) }
    collect(t)
    val rest = new scala.util.Random(seed).shuffle(nodes.tail.toSeq)
    val order = (nodes.head +: rest).toIndexedSeq
    // identity-based index lookup (case classes with equal fields must stay distinct)
    def slot(n: T): Int = order.indexWhere(_ eq n) * 3
    order.flatMap(n => Seq(n.value, n.left.map(slot).getOrElse(-1), n.right.map(slot).getOrElse(-1)))
  }

  private lazy val treeHeightCode = compilerA5(A5.treeHeight)
  private def runTreeHeight(array: Seq[Int]): Int =
    decodeSigned(A4.loadAndRunArray(treeHeightCode, wordsOf(array*)).reg(3)).toInt

  private def checkTree(t: T, name: String): Unit = {
    val expected = height(t)
    assert(runTreeHeight(encodePreorder(t)) == expected, s"$name (preorder layout)")
    for (seed <- 1L to 3L)
      assert(runTreeHeight(encodeShuffled(t, seed)) == expected, s"$name (shuffled layout, seed $seed)")
  }

  test("treeHeight: example from the assignment handout") {
    val handout = Seq(77, 3, 6, 22, -1, -1, -8, 9, 12, -36, -1, -1, 999, -1, -1)
    assert(runTreeHeight(handout) == 3)
  }

  test("treeHeight: single node") {
    assert(runTreeHeight(Seq(5, -1, -1)) == 1)
    assert(runTreeHeight(Seq(0, -1, -1)) == 1)
  }

  test("treeHeight: node values of -1 are not 'no child'") {
    // A node *storing* -1 must still count; only child fields use -1 as a sentinel.
    assert(runTreeHeight(Seq(-1, -1, -1)) == 1)
    checkTree(node(-1, leaf(-1), onlyLeft(-1, leaf(-1))), "all -1 values")
  }

  test("treeHeight: two nodes, only a left or only a right child") {
    checkTree(onlyLeft(1, leaf(2)), "only left")
    checkTree(onlyRight(1, leaf(2)), "only right")
  }

  test("treeHeight: root with two leaves") {
    checkTree(node(1, leaf(2), leaf(3)), "root + 2 leaves")
  }

  test("treeHeight: deeper subtree on the right vs the left") {
    checkTree(node(0, leaf(1), onlyRight(2, onlyRight(3, leaf(4)))), "right deeper")
    checkTree(node(0, onlyLeft(1, onlyLeft(2, leaf(3))), leaf(4)), "left deeper")
  }

  test("treeHeight: zig-zag chain") {
    val zigzag = onlyLeft(1, onlyRight(2, onlyLeft(3, onlyRight(4, leaf(5)))))
    checkTree(zigzag, "zig-zag")
  }

  test("treeHeight: perfect binary trees") {
    def perfect(h: Int, v: Int = 0): T = if (h == 1) leaf(v) else node(v, perfect(h - 1, 2 * v + 1), perfect(h - 1, 2 * v + 2))
    for (h <- 1 to 5) checkTree(perfect(h), s"perfect height $h")
  }

  test("treeHeight: deep left and right chains (recursion depth)") {
    def leftChain(n: Int): T = if (n == 1) leaf(n) else onlyLeft(n, leftChain(n - 1))
    def rightChain(n: Int): T = if (n == 1) leaf(n) else onlyRight(n, rightChain(n - 1))
    for (n <- Seq(10, 50, 100)) {
      assert(runTreeHeight(encodePreorder(leftChain(n))) == n, s"left chain $n")
      assert(runTreeHeight(encodePreorder(rightChain(n))) == n, s"right chain $n")
    }
  }

  test("treeHeight: large and negative node values") {
    checkTree(node(2147483647, leaf(-2147483648), onlyLeft(-999, leaf(0))), "extreme values")
  }

  test("treeHeight: random trees vs Scala reference") {
    val rnd = new scala.util.Random(241)
    def randomTree(size: Int): T =
      if (size <= 1) leaf(rnd.between(-1000, 1000))
      else {
        val leftSize = rnd.nextInt(size) // 0 .. size-1 nodes on the left
        val rightSize = size - 1 - leftSize
        T(rnd.between(-1000, 1000),
          if (leftSize > 0) Some(randomTree(leftSize)) else None,
          if (rightSize > 0) Some(randomTree(rightSize)) else None)
      }
    for (k <- 1 to 20) checkTree(randomTree(rnd.between(1, 30)), s"random tree #$k")
  }
}
