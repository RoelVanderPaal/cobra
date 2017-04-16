package algebra

import algebra.Algebra._
import org.scalatest.{FunSpec, Matchers}

class GradientSpec extends FunSpec with Matchers {
  describe("Gradient should handle gradient of") {

    val ROWS = 3
    val COLS = 5
    val COLS2 = 7

    val V1 = v('V1, ROWS)
    val V2 = v('V2, ROWS)
    val V3 = v('V3, ROWS)
    val U1 = v('U1, 1, COLS)
    val U2 = v('U2, 1, COLS)
    val U3 = v('U3, 1, COLS)
    val A1 = v('A1, ROWS, COLS)
    val A2 = v('A2, ROWS, COLS)
    val A3 = v('A3, ROWS, COLS)
    val B1 = v('B1, COLS, COLS2)

    it("vector") {
      V1.sumR.grad(V1) shouldBe Some(Ones(ROWS, 1))
      V1.sumR.grad(V2) shouldBe None
      U1.sumC.grad(U1) shouldBe Some(Ones(1, COLS))
      U1.sumC.grad(U2) shouldBe None
    }
    it("matrix") {
      A1.sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      A1.sumR.sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      A1.sumC.sumR.grad(A2) shouldBe None
      A1.sumR.sumC.grad(A2) shouldBe None
    }
    it("sum") {
      (A1 + A2).sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      (A1 + A2).sumR.sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      (A1 + A2).sumC.sumR.grad(A2) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      (A1 + A2).sumR.sumC.grad(A2) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      (A1 + A2).sumC.sumR.grad(A3) shouldBe None
      (A1 + A2).sumR.sumC.grad(A3) shouldBe None

      (V1 + V2).sumR.grad(V1) shouldBe Some(Ones(ROWS, 1))
      (V1 + V2).sumR.grad(V2) shouldBe Some(Ones(ROWS, 1))
      (V1 + V2).sumR.grad(V3) shouldBe None

      (U1 + U2).sumC.grad(U1) shouldBe Some(Ones(1, COLS))
      (U1 + U2).sumC.grad(U2) shouldBe Some(Ones(1, COLS))
      (U1 + U2).sumC.grad(U3) shouldBe None

      (A1.sumC + V1).sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      (A1.sumC + V1).sumR.grad(V1) shouldBe Some(Ones(ROWS, 1))
      (A1.sumC + V1).sumR.grad(A2) shouldBe None
      (A1.sumC + V1).sumR.grad(V2) shouldBe None

      (A1.sumR + U1).sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      (A1.sumR + U1).sumC.grad(U1) shouldBe Some(Ones(1, COLS))
      (A1.sumR + U1).sumC.grad(A2) shouldBe None
      (A1.sumR + U1).sumC.grad(U2) shouldBe None

      (A1 + A1).sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS) + Ones(ROWS, 1).broadcastC(COLS))
    }
    it("subtraction") {
      (A1 - A2).sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      (A1 - A2).sumR.sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      (A1 - A2).sumC.sumR.grad(A2) shouldBe Some(-Ones(ROWS, 1).broadcastC(COLS))
      (A1 - A2).sumR.sumC.grad(A2) shouldBe Some(-Ones(1, COLS).broadcastR(ROWS))
      (A1 - A2).sumC.sumR.grad(A3) shouldBe None
      (A1 - A2).sumR.sumC.grad(A3) shouldBe None

      (V1 - V2).sumR.grad(V1) shouldBe Some(Ones(ROWS, 1))
      (V1 - V2).sumR.grad(V2) shouldBe Some(-Ones(ROWS, 1))
      (V1 - V2).sumR.grad(V3) shouldBe None

      (U1 - U2).sumC.grad(U1) shouldBe Some(Ones(1, COLS))
      (U1 - U2).sumC.grad(U2) shouldBe Some(-Ones(1, COLS))
      (U1 - U2).sumC.grad(U3) shouldBe None

      (A1.sumC - V1).sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS))
      (A1.sumC - V1).sumR.grad(V1) shouldBe Some(-Ones(ROWS, 1))
      (A1.sumC - V1).sumR.grad(A2) shouldBe None
      (A1.sumC - V1).sumR.grad(V2) shouldBe None

      (A1.sumR - U1).sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS))
      (A1.sumR - U1).sumC.grad(U1) shouldBe Some(-Ones(1, COLS))
      (A1.sumR - U1).sumC.grad(A2) shouldBe None
      (A1.sumR - U1).sumC.grad(U2) shouldBe None

      (A1 - A1).sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS) - Ones(ROWS, 1).broadcastC(COLS))
    }
    it("element-wise multiplication") {
      (A1 :* A2).sumC.sumR.grad(A1) shouldBe Some(A2 :* Ones(ROWS, 1).broadcastC(COLS))
      (A1 :* A2).sumR.sumC.grad(A1) shouldBe Some(A2 :* Ones(1, COLS).broadcastR(ROWS))
      (A1 :* A2).sumC.sumR.grad(A2) shouldBe Some(A1 :* Ones(ROWS, 1).broadcastC(COLS))
      (A1 :* A2).sumR.sumC.grad(A2) shouldBe Some(A1 :* Ones(1, COLS).broadcastR(ROWS))
      (A1 :* A2).sumC.sumR.grad(A3) shouldBe None
      (A1 :* A2).sumR.sumC.grad(A3) shouldBe None

      (V1 :* V2).sumR.grad(V1) shouldBe Some(V2 :* Ones(ROWS, 1))
      (V1 :* V2).sumR.grad(V2) shouldBe Some(V1 :* Ones(ROWS, 1))
      (V1 :* V2).sumR.grad(V3) shouldBe None

      (U1 :* U2).sumC.grad(U1) shouldBe Some(U2 :* Ones(1, COLS))
      (U1 :* U2).sumC.grad(U2) shouldBe Some(U1 :* Ones(1, COLS))
      (U1 :* U2).sumC.grad(U3) shouldBe None

      (A1.sumC :* V1).sumR.grad(A1) shouldBe Some(BroadcastC(V1 :* Ones(ROWS, 1), COLS))
      (A1.sumC :* V1).sumR.grad(V1) shouldBe Some(A1.sumC :* Ones(ROWS, 1))
      (A1.sumC :* V1).sumR.grad(A2) shouldBe None
      (A1.sumC :* V1).sumR.grad(V2) shouldBe None

      (A1.sumR :* U1).sumC.grad(A1) shouldBe Some(BroadcastR(U1 :* Ones(1, COLS), ROWS))
      (A1.sumR :* U1).sumC.grad(U1) shouldBe Some(A1.sumR :* Ones(1, COLS))
      (A1.sumR :* U1).sumC.grad(A2) shouldBe None
      (A1.sumR :* U1).sumC.grad(U2) shouldBe None

      (A1 :* A1).sumC.sumR.grad(A1) shouldBe Some((A1 :* Ones(ROWS, 1).broadcastC(COLS)) + (A1 :* Ones(ROWS, 1).broadcastC(COLS)))
    }
    it("element-wise division") {
      (A1 :/ A2).sumC.sumR.grad(A1) shouldBe Some(A2 :* Ones(ROWS, 1).broadcastC(COLS) :/ (A2 :* A2))
      (A1 :/ A2).sumR.sumC.grad(A1) shouldBe Some(A2 :* Ones(1, COLS).broadcastR(ROWS) :/ (A2 :* A2))
      (A1 :/ A2).sumC.sumR.grad(A2) shouldBe Some(-(A1 :* Ones(ROWS, 1).broadcastC(COLS) :/ (A2 :* A2)))
      (A1 :/ A2).sumR.sumC.grad(A2) shouldBe Some(-(A1 :* Ones(1, COLS).broadcastR(ROWS) :/ (A2 :* A2)))
      (A1 :/ A2).sumC.sumR.grad(A3) shouldBe None
      (A1 :/ A2).sumR.sumC.grad(A3) shouldBe None

      (V1 :/ V2).sumR.grad(V1) shouldBe Some(V2 :* Ones(ROWS, 1) :/ (V2 :* V2))
      (V1 :/ V2).sumR.grad(V2) shouldBe Some(-(V1 :* Ones(ROWS, 1) :/ (V2 :* V2)))
      (V1 :/ V2).sumR.grad(V3) shouldBe None

      (U1 :/ U2).sumC.grad(U1) shouldBe Some(U2 :* Ones(1, COLS) :/ (U2 :* U2))
      (U1 :/ U2).sumC.grad(U2) shouldBe Some(-(U1 :* Ones(1, 5) :/ (U2 :* U2)))
      (U1 :/ U2).sumC.grad(U3) shouldBe None

      (A1.sumC :/ V1).sumR.grad(A1) shouldBe Some((V1 :* Ones(ROWS, 1) :/ (V1 :* V1)).broadcastC(COLS))
      (A1.sumC :/ V1).sumR.grad(V1) shouldBe Some(-(A1.sumC :* Ones(ROWS, 1) :/ (V1 :* V1)))
      (A1.sumC :/ V1).sumR.grad(A2) shouldBe None
      (A1.sumC :/ V1).sumR.grad(V2) shouldBe None

      (A1.sumR :/ U1).sumC.grad(A1) shouldBe Some((U1 :* Ones(1, COLS) :/ (U1 :* U1)).broadcastR(ROWS))
      (A1.sumR :/ U1).sumC.grad(U1) shouldBe Some(-(A1.sumR :* Ones(1, COLS) :/ (U1 :* U1)))
      (A1.sumR :/ U1).sumC.grad(A2) shouldBe None
      (A1.sumR :/ U1).sumC.grad(U2) shouldBe None

      (A1 :/ A1).sumC.sumR.grad(A1) shouldBe Some(((A1 :* Ones(ROWS, 1).broadcastC(COLS)) :/ (A1 :* A1)) - ((A1 :* Ones(ROWS, 1).broadcastC(COLS)) :/ (A1 :* A1)))
    }
    it("multiplication") {
      (A1 * B1).sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS2) * B1.t)
      (A1 * B1).sumR.sumC.grad(A1) shouldBe Some(Ones(1, COLS2).broadcastR(ROWS) * B1.t)
      (A1 * B1).sumC.sumR.grad(B1) shouldBe Some(A1.t * Ones(ROWS, 1).broadcastC(COLS2))
      (A1 * B1).sumR.sumC.grad(B1) shouldBe Some(A1.t * Ones(1, COLS2).broadcastR(ROWS))
      (A1 * B1).sumC.sumR.grad(A3) shouldBe None
      (A1 * B1).sumR.sumC.grad(A3) shouldBe None

      (V1 * U1).sumR.grad(V1) shouldBe Some(Ones(ROWS, COLS) * U1.t)
      //      (V1 * U1).sumR.grad(U1) shouldBe Some(V1.t * Ones(ROWS, COLS))
      (V1 * U1).sumR.grad(V3) shouldBe None

      (U1 :* U2).sumC.grad(U1) shouldBe Some(U2 :* Ones(1, COLS))
      (U1 :* U2).sumC.grad(U2) shouldBe Some(U1 :* Ones(1, COLS))
      (U1 :* U2).sumC.grad(U3) shouldBe None

      (A1.sumC :* V1).sumR.grad(A1) shouldBe Some(BroadcastC(V1 :* Ones(ROWS, 1), COLS))
      (A1.sumC :* V1).sumR.grad(V1) shouldBe Some(A1.sumC :* Ones(ROWS, 1))
      (A1.sumC :* V1).sumR.grad(A2) shouldBe None
      (A1.sumC :* V1).sumR.grad(V2) shouldBe None

      (A1.sumR :* U1).sumC.grad(A1) shouldBe Some(BroadcastR(U1 :* Ones(1, COLS), ROWS))
      (A1.sumR :* U1).sumC.grad(U1) shouldBe Some(A1.sumR :* Ones(1, COLS))
      (A1.sumR :* U1).sumC.grad(A2) shouldBe None
      (A1.sumR :* U1).sumC.grad(U2) shouldBe None

      (A1 :* A1).sumC.sumR.grad(A1) shouldBe Some((A1 :* Ones(ROWS, 1).broadcastC(COLS)) + (A1 :* Ones(ROWS, 1).broadcastC(COLS)))
    }
    it("negate") {
      (-A1).sumC.sumR.grad(A1) shouldBe Some(-Ones(ROWS, 1).broadcastC(COLS))
      (-A1).sumR.sumC.grad(A1) shouldBe Some(-Ones(1, COLS).broadcastR(ROWS))
      (-A1).sumC.sumR.grad(A2) shouldBe None
      (-A1).sumR.sumC.grad(A2) shouldBe None
    }
    it("transpose") {
      A1.t.t.sumC.sumR.grad(A1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS).t.t)
      A1.t.t.sumR.sumC.grad(A1) shouldBe Some(Ones(1, COLS).broadcastR(ROWS).t.t)
      A1.t.t.sumC.sumR.grad(A2) shouldBe None
      A1.t.t.sumR.sumC.grad(A2) shouldBe None
    }
    it("broadcast") {
      U1.broadcastR(ROWS).sumC.sumR.grad(U1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS).sumR)
      V1.broadcastC(COLS).sumC.sumR.grad(V1) shouldBe Some(Ones(ROWS, 1).broadcastC(COLS).sumC)
      U1.broadcastR(ROWS).sumC.sumR.grad(U2) shouldBe None
      V1.broadcastC(COLS).sumC.sumR.grad(V2) shouldBe None
    }
  }
}
