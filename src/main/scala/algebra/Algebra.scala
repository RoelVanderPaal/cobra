package algebra

import scala.language.implicitConversions

sealed trait Expr {
  def rows: Int

  def cols: Int

  def size: Int = rows * cols

  def +(e: Expr) = AddOp(this, e)

  def -(e: Expr) = SubOp(this, e)

  def *(e: Expr) = MulOp(this, e)

  def :*(e: Expr) = EMulOp(this, e)

  def :/(e: Expr) = EDivOp(this, e)

  def unary_- = Negate(this)

}

case class Var(name: Symbol, rows: Int = 1, cols: Int = 1) extends Expr

case class AddOp(e1: Expr, e2: Expr) extends Expr {
  assert(e1.rows == e2.rows, s"AddOp: number of rows ${e1.rows} != ${e2.rows} for $e1 and $e2")
  assert(e1.cols == e2.cols, s"AddOp: number of cols ${e1.cols} != ${e2.cols} for $e1 and $e2")

  override def rows: Int = e1.rows

  override def cols: Int = e1.cols

}

case class SubOp(e1: Expr, e2: Expr) extends Expr {
  assert(e1.rows == e2.rows, s"SubOp: number of rows ${e1.rows} != ${e2.rows} for $e1 and $e2")
  assert(e1.cols == e2.cols, s"SubOp: number of cols ${e1.cols} != ${e2.cols} for $e1 and $e2")

  override def rows: Int = e1.rows

  override def cols: Int = e1.cols

}

case class MulOp(e1: Expr, e2: Expr) extends Expr {
  assert(e1.cols == e2.rows, s"MulOp: number of cols of first expr does not match number of rows of second expr: ${e1.cols} != ${e2.rows} for $e1 and $e2")

  override def rows: Int = e1.rows

  override def cols: Int = e2.cols
}

case class EMulOp(e1: Expr, e2: Expr) extends Expr {
  assert(e1.rows == e2.rows, s"EMulOp: number of rows ${e1.rows} != ${e2.rows} for $e1 and $e2")
  assert(e1.cols == e2.cols, s"EMulOp: number of cols ${e1.cols} != ${e2.cols} for $e1 and $e2")

  override def rows: Int = e1.rows

  override def cols: Int = e1.cols

}

case class EDivOp(e1: Expr, e2: Expr) extends Expr {
  assert(e1.rows == e2.rows, s"EDivOp: number of rows ${e1.rows} != ${e2.rows} for $e1 and $e2")
  assert(e1.cols == e2.cols, s"EDivOp: number of cols ${e1.cols} != ${e2.cols} for $e1 and $e2")

  override def rows: Int = e1.rows

  override def cols: Int = e1.cols

}

case class Negate(e: Expr) extends Expr {
  override def rows: Int = e.rows

  override def cols: Int = e.cols

}

object Algebra {
  implicit def symbolToVar(s: Symbol): Var = Var(s)

  def v(s: Symbol, rows: Int = 1, cols: Int = 1) = Var(s, rows, cols)
}
