package algebra

import algebra.Algebra._
import org.scalatest.{FunSpec, Matchers}

class AlgebraSpec extends FunSpec with Matchers {
  describe("In Algebra") {

    val a = v('a, 2, 3)
    val b = v('b, 2, 3)
    val c = v('c, 4, 3)
    val d = v('c, 2, 4)

    describe("Var creation should be possible") {
      it("implicitly") {
        val a: Var = 'a
        a shouldBe Var('a)
      }
      it("explicitly") {
        v('a, 2, 3) shouldBe Var('a, 2, 3)
      }
    }
    describe("expression") {
      it("should have correct size") {
        v('a, 2, 3).size shouldBe 2 * 3
      }
    }
    describe("addition") {
      it("should create AddOp") {
        val r = a + b
        r shouldBe AddOp(a, b)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy a + c should have message "assertion failed: AddOp: number of rows 2 != 4 for Var('a,2,3) and Var('c,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy a + d should have message "assertion failed: AddOp: number of cols 3 != 4 for Var('a,2,3) and Var('c,2,4)"
      }
    }
    describe("subtraction") {
      it("should create SubOp") {
        val r = a - b
        r shouldBe SubOp(a, b)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy a - c should have message "assertion failed: SubOp: number of rows 2 != 4 for Var('a,2,3) and Var('c,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy a - d should have message "assertion failed: SubOp: number of cols 3 != 4 for Var('a,2,3) and Var('c,2,4)"
      }
    }
    describe("element-wise multiplication") {
      it("should create EMulOp") {
        val r = a :* b
        r shouldBe EMulOp(a, b)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy a :* c should have message "assertion failed: EMulOp: number of rows 2 != 4 for Var('a,2,3) and Var('c,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy a :* d should have message "assertion failed: EMulOp: number of cols 3 != 4 for Var('a,2,3) and Var('c,2,4)"
      }
    }
    describe("element-wise division") {
      it("should create EDivOp") {
        val r = a :/ b
        r shouldBe EDivOp(a, b)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
      it("should fail when number of rows do not match") {
        the[AssertionError] thrownBy a :/ c should have message "assertion failed: EDivOp: number of rows 2 != 4 for Var('a,2,3) and Var('c,4,3)"
      }
      it("should fail when number of cols do not match") {
        the[AssertionError] thrownBy a :/ d should have message "assertion failed: EDivOp: number of cols 3 != 4 for Var('a,2,3) and Var('c,2,4)"
      }
    }
    describe("multiplication") {
      it("should create MulOp") {
        val b = v('b, 3, 4)
        val r = a * b
        r shouldBe MulOp(a, b)
        r.rows shouldBe 2
        r.cols shouldBe 4
      }
      it("should fail when number of cols of first expressions do not match number of rows of second expression") {
        val c = v('c, 4, 3)
        the[AssertionError] thrownBy a * c should have message "assertion failed: MulOp: number of cols of first expr does not match number of rows of second expr: 3 != 4 for Var('a,2,3) and Var('c,4,3)"
      }
    }
    describe("negation"){
      it("should create NegOp"){
        val r = -a
        r shouldBe Negate(a)
        r.rows shouldBe 2
        r.cols shouldBe 3
      }
    }

  }
}
