package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Star(exp: Expression) extends Expression {
    override def toString = if (!exp.atomic) "("+exp+")*" else exp+"*"

    override def normalize = this match {
        case Star(Star(ex)) => ex.normalize
        case Star(Oplus(e1, e2)) => Circle(Star(e1.normalize), Star(e2.normalize)).normalize
        case Star(Circle(ex1, ex2)) => Circle(Star(ex1.normalize), Star(ex2.normalize)).normalize
        case _ => Star(exp.normalize)
    }
}
