package algebra

trait LatexPrinter {
  this: Expr =>

  def lprint: String = this match {
    case Var(name, _, _) => name.name
    case _: Ones => s"I"
    case AddOp(e1, e2) => s"(${e1.lprint} + ${e2.lprint})"
    case SubOp(e1, e2) => s"(${e1.lprint} - ${e2.lprint})"
    case EMulOp(e1, e2) => s"${e1.lprint} \\circ ${e2.lprint}" // https://en.wikipedia.org/wiki/Hadamard_product_(matrices)
    case EDivOp(e1, e2) => s"${e1.lprint} \\oslash ${e2.lprint}" // https://en.wikipedia.org/wiki/Hadamard_product_(matrices)
    case MulOp(e1, e2) => s"${e1.lprint}${e2.lprint}"
    case SumR(e1) => s"\\sum_{i} ${e1.lprint}_{ij}"
    case SumC(e1) => s"\\sum_{j} ${e1.lprint}_{ij}"
    case Transp(e1) => s"${e1.lprint}^T"
    case Negate(e1) => s"-${e1.lprint}"
    case UnOp(o, e1) => s"${
      o match {
        case Sin => "sin"
        case Cos => "cos"
        case Exp => "exp"
      }
    }(${e1.lprint})"
    case _: BroadcastC => throw new UnsupportedOperationException // TODO lookup suitable latex symbol
    case _: BroadcastR => throw new UnsupportedOperationException // TODO lookup suitable latex symbol
  }
}
