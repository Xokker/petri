package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Star(exp: Expression) extends Expression {
    override def toString = exp+"*"

    override def normalize = this match {
        case Star(Star(ex)) => ex.normalize
        case Star(Oplus(e1, e2)) => Circle(Star(e1.normalize), Star(e2.normalize)).normalize
        case Star(Circle(ex1, Star(ex))) => Circle(Star(ex1.normalize), Star(ex.normalize)).normalize
        case Star(Circle(Star(ex), ex2)) => Star(Circle(ex2, Star(ex))).normalize
        case Star(Empty()) => Empty()
        case _ => Star(exp.normalize)
    }
}
