package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 03.05.2014
 */
abstract class Expression {
    def normalize: Expression = this
}

case class Star(exp: Expression) extends Expression {
    override def toString = "("+exp+")*"
}

case class Oplus(left: Expression, right: Expression) extends Expression {
    override def toString = "("+left+"⊕"+right+")"
}

case class Circle(left: Expression, right: Expression) extends Expression {
    override def toString = "("+left+"◦"+right+")"

    override def normalize: Expression = this match {
        case Circle(_, Oplus(oleft, oright)) => Oplus(Circle(left.normalize, oleft.normalize), Circle(left.normalize, oright.normalize))
        case Circle(Oplus(oleft, oright), _) => Circle(right, left).normalize
        case _ => this
    }
}

case class Input(id: String) extends Expression {
    override def toString = "?"+id
}

case class Output(id: String) extends Expression {
    override def toString = "!"+id
}

object Expression {
    def main(args: Array[String]) {
        val expression: Expression = Circle(Star(Circle(Output("CD"), Input("CR"))), Oplus(Circle(Star(Input("A")),
            Output("E")), Circle(Output("CD"), Output("E"))))

        println(expression)

        println(expression.normalize)
    }
}