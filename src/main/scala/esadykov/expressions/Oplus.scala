package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Oplus(left: Expression, right: Expression) extends Expression {
    override def toString = toString(false)

    override def toString(noParen: Boolean) = {
        val l = if (left.isInstanceOf[Oplus]) left.toString(true) else left.toString(false)
        val r = if (right.isInstanceOf[Oplus]) right.toString(true) else right.toString(false)
        if (noParen) l + "⊕" + r
        else "(" + l + "⊕" + r + ")"
    }

    override def normalize =
        if (left == right) left.normalize
        else this match {
            case Oplus(_, Empty()) => left.normalize
            case Oplus(Empty(), _) => right.normalize
            case _ => Oplus(left.normalize, right.normalize)
        }
}
