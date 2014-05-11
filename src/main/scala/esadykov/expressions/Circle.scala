package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Circle(left: Expression, right: Expression) extends Expression {
    override def toString = toString(false)

    override def toString(noParen: Boolean) = {
        val l = if (left.isInstanceOf[Circle]) left.toString(true) else left.toString(false)
        val r = if (right.isInstanceOf[Circle]) right.toString(true) else right.toString(false)
        if (noParen) l + "◦" + r
        else "(" + l + "◦" + r + ")"
    }

    override def normalize = this match {
        case Circle(_, Oplus(oleft, oright)) => Oplus(Circle(left.normalize, oleft.normalize), Circle(left.normalize, oright.normalize)).normalize
        case Circle(Oplus(oleft, oright), _) => Circle(right, left).normalize
        case Circle(_, Empty()) => left.normalize
        case Circle(Empty(), _) => right.normalize
        case _ => Circle(left.normalize, right.normalize)
    }
}
