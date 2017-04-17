package algebra

import scala.language.implicitConversions

sealed trait Expr extends Gradient with LatexPrinter {
  def rows: Int

  def cols: Int

  def size: Int = rows * cols

  def +(e: Expr) = AddOp(this, e)

  def -(e: Expr) = SubOp(this, e)

  def *(e: Expr) = MulOp(this, e)

  def :*(e: Expr) = EMulOp(this, e)

  def :/(e: Expr) = EDivOp(this, e)

  def unary_- = Negate(this)

  def sumR = SumR(this)

  def sumC = SumC(this)

  def broadcastR(rows: Int) = BroadcastR(this, rows)

  def broadcastC(cols: Int) = BroadcastC(this, cols)

  def t = Transp(this)
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

case class SumR(e: Expr) extends Expr {
  override def rows: Int = 1

  override def cols: Int = e.cols
}

case class SumC(e: Expr) extends Expr {
  override def rows: Int = e.rows

  override def cols: Int = 1
}

case class Transp(e: Expr) extends Expr {
  override def rows: Int = e.cols

  override def cols: Int = e.rows
}


case class BroadcastR(e: Expr, rows: Int) extends Expr {
  assert(e.rows == 1, s"BroadcastR: number of rows should be 1 but was ${e.rows} for $e")

  override def cols: Int = e.cols
}

case class BroadcastC(e: Expr, cols: Int) extends Expr {
  assert(e.cols == 1, s"BroadcastC: number of cols should be 1 but was ${e.cols} for $e")

  override def rows: Int = e.rows
}


case class Ones(rows: Int, cols: Int) extends Expr

sealed trait UnOperation

case object Sin extends UnOperation

case object Cos extends UnOperation

case object Exp extends UnOperation

case class UnOp(o: UnOperation, e: Expr) extends Expr {
  override def rows: Int = e.rows

  override def cols: Int = e.cols
}


object Algebra {
  implicit def symbolToVar(s: Symbol): Var = Var(s)

  def v(s: Symbol, rows: Int = 1, cols: Int = 1) = Var(s, rows, cols)

  def ones(rows: Int = 1, cols: Int = 1) = Ones(rows, cols)

  def sin(e: Expr) = UnOp(Sin, e)

  def cos(e: Expr) = UnOp(Cos, e)

  def exp(e: Expr) = UnOp(Exp, e)
}
