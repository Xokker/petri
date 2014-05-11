package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Oplus(left: Expression, right: Expression) extends Expression {
    override def toString = "("+left+"⊕"+right+")"

    override def normalize =
        if (left == right) left.normalize
        else this match {
            case Oplus(_, Empty()) => left.normalize
            case Oplus(Empty(), _) => right.normalize
            case _ => Oplus(left.normalize, right.normalize)
        }
}
