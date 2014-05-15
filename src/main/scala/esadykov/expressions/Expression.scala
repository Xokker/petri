package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 03.05.2014
 */
abstract class Expression {
    def normalize(): Expression = this

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

    private def componentsForAlgebra(components: List[Expression], acc: Map[String, List[String]], counter: Int): Map[String, List[String]] = {
        def newAccSingle(id: String) = {
            val set: List[String] = acc.getOrElse(id, Nil) :+ "1"
            acc + (id -> set)
        }

        components match {
            case x :: xs => x match {
                case Star(ex) =>
                    // TODO: make functional
                    var newAc: Map[String, List[String]] = acc
                    for (e <- Expression.components(ex).head if e.isInstanceOf[Input] || e.isInstanceOf[Output]) {
                        val id: String =
                            e match {
                                case input: Input => input.id
                                case output: Output => e.asInstanceOf[Output].id
                            }
                        val set: List[String] = newAc.getOrElse(id, Nil) :+ ("n" + counter)
                        newAc = newAc + (id -> set)
                    }
                    componentsForAlgebra(xs, newAc, counter + 1)
                case Input(id) => componentsForAlgebra(xs, newAccSingle(id), counter)
                case Output(id) => componentsForAlgebra(xs, newAccSingle(id), counter)
                case _ => throw new IllegalStateException("element: "+x)
            }
            case Nil => acc
        }
    }

    def componentsForAlgebra(components: List[Expression]): Map[String, List[String]] =
        componentsForAlgebra(components, Map.empty[String, List[String]], 1)


    def main(args: Array[String]) {
        val expression: Expression = Circle(Star(Circle(Output("CD"),
                                                        Input("CR"))),
                                            Oplus(Circle(Star(Input("A")),
                                                         Output("E")),
                                                  Circle(Output("CD"),
                                                         Output("E"))))
        val normalized: Expression = expression.normalize()
        val components1: List[List[Expression]] = components(normalized)
        println(components1.mkString("\n"))

        println(componentsForAlgebra(components1.head))
    }
}
