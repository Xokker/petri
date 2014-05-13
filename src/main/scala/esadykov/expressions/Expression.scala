package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 03.05.2014
 */
abstract class Expression {
    def normalize(): Expression = this

    def atomic: Boolean = false

    def toString(noParen: Boolean): String = toString
}

object Expression {
    def components(expression: Expression): List[List[Expression]] = {

        def innerComponents(expr: Expression): List[Expression] = {
            expr match {
                case Circle(left, right) => innerComponents(left) ++ innerComponents(right)
                case _ => List(expr)
            }
        }

        expression match {
            case Oplus(left, right) => components(left) ++ components(right)
            case _ => List[List[Expression]](innerComponents(expression))
        }
    }


    def main(args: Array[String]) {
        val expression: Expression = Circle(Star(Circle(Output("CD"), Input("CR"))),
            Oplus(Circle(Star(Input("A")), Output("E")), Circle(Output("CD"), Output("E"))))

        println(expression)

        val normalized: Expression = expression.normalize
        println(normalized)

        println(components(normalized).mkString("\n"))
    }
}
