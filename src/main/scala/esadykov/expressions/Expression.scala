package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 03.05.2014
 */
abstract class Expression {
    def normalize: Expression = this

    def atomic: Boolean = false

    // TODO
//    def components: Array[Component] = {
//        def extract(acc: Array[Component]) {
//            this match {
//                case Input(id) => acc + new Component(id, false, true)
//                case Oplus(left, right) => left match {
//                    case Oplus(innerLeft, innerRight) =>
//                }
//            }
//        }
//    }
}

object Expression {
    def main(args: Array[String]) {
        val expression: Expression = Circle(Star(Circle(Output("CD"), Input("CR"))),
            Oplus(Circle(Star(Input("A")), Output("E")), Circle(Output("CD"), Output("E"))))

        println(expression)

        println(expression.normalize)
    }
}