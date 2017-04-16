package algebra

import algebra.Algebra._
import org.scalatest.{FunSpec, Matchers}

class AlgebraSpec extends FunSpec with Matchers {
  describe("In Algebra") {

    val A = v('A, 2, 3)
    val B = v('B, 2, 3)
    val C = v('C, 4, 3)
    val D = v('D, 2, 4)
    val U = v('U, 1, 4)
    val V = v('V, 4)

    describe("Var creation should be possible") {
      it("implicitly") {
        val a: Var = 'a
        a shouldBe Var('a)
      }
      it("explicitly") {
        A shouldBe Var('A, 2, 3)
      }
    }
    describe("expression") {
      it("should have correct size") {
        A.size shouldBe 2 * 3
      }
    }
    describe("addition") {
      it("should create AddOp") {
        val r = A + B
        r shouldBe AddOp(A, B)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy A + C should have message "assertion failed: AddOp: number of rows 2 != 4 for Var('A,2,3) and Var('C,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy A + D should have message "assertion failed: AddOp: number of cols 3 != 4 for Var('A,2,3) and Var('D,2,4)"
      }
    }
    describe("subtraction") {
      it("should create SubOp") {
        val r = A - B
        r shouldBe SubOp(A, B)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy A - C should have message "assertion failed: SubOp: number of rows 2 != 4 for Var('A,2,3) and Var('C,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy A - D should have message "assertion failed: SubOp: number of cols 3 != 4 for Var('A,2,3) and Var('D,2,4)"
      }
    }
    describe("element-wise multiplication") {
      it("should create EMulOp") {
        val r = A :* B
        r shouldBe EMulOp(A, B)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy A :* C should have message "assertion failed: EMulOp: number of rows 2 != 4 for Var('A,2,3) and Var('C,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy A :* D should have message "assertion failed: EMulOp: number of cols 3 != 4 for Var('A,2,3) and Var('D,2,4)"
      }
    }
    describe("element-wise division") {
      it("should create EDivOp") {
        val r = A :/ B
        r shouldBe EDivOp(A, B)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy A :/ C should have message "assertion failed: EDivOp: number of rows 2 != 4 for Var('A,2,3) and Var('C,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy A :/ D should have message "assertion failed: EDivOp: number of cols 3 != 4 for Var('A,2,3) and Var('D,2,4)"
      }
    }
    describe("multiplication") {
      it("should create MulOp") {
        val b = v('b, 3, 4)
        val r = A * b
        r shouldBe MulOp(A, b)
        r.rows shouldBe 2
        r.cols shouldBe 4
      }
      it("should fail when number of cols of first expressions do not match number of rows of second expression") {
        the[AssertionError] thrownBy A * C should have message "assertion failed: MulOp: number of cols of first expr does not match number of rows of second expr: 3 != 4 for Var('A,2,3) and Var('C,4,3)"
      }
    }
    describe("negation") {
      it("should create NegOp") {
        val r = -A
        r shouldBe Negate(A)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
    }
    describe("row summation") {
      it("should create SumR") {
        val r = A.sumR
        r shouldBe SumR(A)
        r.rows shouldBe 1
        r.cols shouldBe 3
      }
    }
    describe("column summation") {
      it("should create SumC") {
        val r = A.sumC
        r shouldBe SumC(A)
        r.rows shouldBe 2
        r.cols shouldBe 1
      }
    }
    describe("row broadcast") {
      it("should create BroadcastR") {
        val r = U.broadcastR(3)
        r shouldBe BroadcastR(U, 3)
        r.rows shouldBe 3
        r.cols shouldBe 4
      }
      it("should fail when number of cols of first expressions do not match number of rows of second expression") {
        the[AssertionError] thrownBy A.broadcastR(5) should have message "assertion failed: BroadcastR: number of rows should be 1 but was 2 for Var('A,2,3)"
      }
    }
    describe("column broadcast") {
      it("should create BroadcastC") {
        val r = V.broadcastC(3)
        r shouldBe BroadcastC(V, 3)
        r.rows shouldBe 4
        r.cols shouldBe 3
      }
      it("should fail when number of cols of first expressions do not match number of rows of second expression") {
        the[AssertionError] thrownBy A.broadcastC(5) should have message "assertion failed: BroadcastC: number of cols should be 1 but was 3 for Var('A,2,3)"
      }
    }
    describe("transpose") {
      it("should create Transp") {
        val r = A.t
        r shouldBe Transp(A)
        r.rows shouldBe 3
        r.cols shouldBe 2
      }
    }
  }
}
