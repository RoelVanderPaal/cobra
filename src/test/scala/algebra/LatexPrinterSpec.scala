package algebra

import algebra.Algebra._
import org.scalatest.{FunSpec, Matchers}

class LatexPrinterSpec extends FunSpec with Matchers {
  describe("LatexPrinter should create latex string of") {

    val ROWS = 3
    val COLS = 5
    val COLS2 = 7

    val A = v('A, ROWS, COLS)
    val B = v('B, ROWS, COLS)
    val C = v('C, COLS, COLS2)

    it("var") {
      A.lprint shouldBe "A"
    }
    it("ones") {
      ones().lprint shouldBe "I"
    }
    it("addition") {
      (A + B).lprint shouldBe "(A + B)"
    }
    it("subtraction") {
      (A - B).lprint shouldBe "(A - B)"
    }
    it("element-wise multiplication") {
      (A :* B).lprint shouldBe "A \\circ B"
    }
    it("element-wise division") {
      (A :/ B).lprint shouldBe "A \\oslash B"
    }
    it("multiplication") {
      (A * C).lprint shouldBe "AC"
    }
    it("summation of rows") {
      A.sumR.lprint shouldBe "\\sum_{i} A_{ij}"
    }
    it("summation of columns") {
      A.sumC.lprint shouldBe "\\sum_{j} A_{ij}"
    }
    it("transpose") {
      A.t.lprint shouldBe "A^T"
    }
    it("negate") {
      (-A).lprint shouldBe "-A"
    }
    it("sin") {
      sin(A).lprint shouldBe "sin(A)"
    }
    it("cos") {
      cos(A).lprint shouldBe "cos(A)"
    }
    it("exp") {
      exp(A).lprint shouldBe "exp(A)"
    }
  }
}
