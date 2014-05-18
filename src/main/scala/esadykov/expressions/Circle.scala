package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Circle(left: Expression, right: Expression) extends Expression {
    private[this] val CircleSymbol = "◦"

    override def toString = toString(false)

    override def toString(noParen: Boolean) = {
        val l = left.toString(noParen = left.isInstanceOf[Circle])
        val r = right.toString(noParen = right.isInstanceOf[Circle])
        if (noParen) l + CircleSymbol + r
        else "(" + l + CircleSymbol + r + ")"
    }

    override def normalize() = this match {
        case Circle(_, Oplus(oleft, oright)) => Oplus(Circle(left.normalize(), oleft.normalize()), Circle(left.normalize(), oright.normalize())).normalize()
        case Circle(Oplus(oleft, oright), _) => Circle(right, left).normalize()
        case Circle(_, Empty()) => left.normalize()
        case Circle(Empty(), _) => right.normalize()
        case _ => Circle(left.normalize(), right.normalize())
    }
}
