package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Circle(left: Expression, right: Expression) extends Expression {
    override def toString = "("+left+"◦"+right+")"

    override def normalize = this match {
        case Circle(_, Oplus(oleft, oright)) => Oplus(Circle(left.normalize, oleft.normalize), Circle(left.normalize, oright.normalize)).normalize
        case Circle(Oplus(oleft, oright), _) => Circle(right, left).normalize
        case Circle(_, Empty()) => left.normalize
        case Circle(Empty(), _) => right.normalize
        case _ => Circle(left.normalize, right.normalize)
    }
}
