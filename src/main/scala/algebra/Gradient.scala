package algebra

import algebra.Algebra.{cos, exp, sin}

trait Gradient {
  this: Expr =>

  def grad(p: Var): Option[Expr] = {
    def g(e: Expr, r: Option[Expr]): Option[Expr] = {
      def handle(f: (Expr, Expr) => Expr)(l: Option[Expr], r: Option[Expr]): Option[Expr] = (l, r) match {
        case (None, None) => None
        case (left: Some[Expr], None) => left
        case (None, right: Some[Expr]) => right
        case (Some(left), Some(right)) => Some(f(left, right))
      }

      def |-|(l: Option[Expr], r: Option[Expr]): Option[Expr] = (l, r) match {
        case (None, None) => None
        case (left: Some[Expr], None) => left
        case (None, right: Some[Expr]) => right.map(-_)
        case (Some(left), Some(right)) => Some(left - right)
      }

      def |+| = handle(_ + _) _

      def |*| = handle(_ * _) _

      def |:*| = handle(_ :* _) _

      e match {
        case Negate(e1) => g(e1, r).map(-_)
        case BroadcastC(e1, _) => g(e1, r.map(_.sumC))
        case BroadcastR(e1, _) => g(e1, r.map(_.sumR))
        case Transp(e1) => g(e1, r.map(_.t))
        case SumR(e1) => g(e1, r.orElse(Some(Ones(e1.rows, e1.cols))).map { n => if (n.rows == e1.rows) n else n.broadcastR(e1.rows) })
        case SumC(e1) => g(e1, r.orElse(Some(Ones(e1.rows, e1.cols))).map { n => if (n.cols == e1.cols) n else n.broadcastC(e1.cols) })
        case AddOp(e1, e2) => |+|(g(e1, r), g(e2, r))
        case SubOp(e1, e2) => |-|(g(e1, r), g(e2, r))
        case EMulOp(e1, e2) => |+|(g(e1, |:*|(Some(e2), r)), g(e2, |:*|(Some(e1), r)))
        case MulOp(e1, e2) => |+|(g(e1, |*|(r, Some(e2.t))), g(e2, |*|(Some(e1.t), r)))
        case EDivOp(e1, e2) => |-|(g(e1, |:*|(Some(e2), r).map(_ :/ (e2 :* e2))), g(e2, |:*|(Some(e1), r).map(_ :/ (e2 :* e2))))
        case UnOp(o, e1) => o match {
          case Sin => g(e1, |:*|(Some(cos(e1)), r))
          case Cos => g(e1, |:*|(Some(-sin(e1)), r))
          case Exp => g(e1, |:*|(Some(exp(e1)), r))
        }
        case v: Var if v == p => r
        case _: Var => None
      }
    }

    g(this, None)
  }
}
