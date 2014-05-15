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

    private def componentsForAlgebra(components: List[Expression], acc: Map[String, Set[String]], counter: Int): Map[String, Set[String]] = {
        def newAccDouble(id1: String, id2: String) = {
            val set1: Set[String] = acc.getOrElse(id1, Set.empty[String]) + ("n" + counter)
            val set2: Set[String] = acc.getOrElse(id2, Set.empty[String]) + ("n" + counter)
            acc + (id1 -> set1) + (id2 -> set2)
        }
        def newAccSingle(id: String) = {
            val set: Set[String] = acc.getOrElse(id, Set.empty[String]) + "1"
            acc + (id -> set)
        }
        def newAccStar(id: String) = {
            val set: Set[String] = acc.getOrElse(id, Set.empty[String]) + ("n" + counter)
            acc + (id -> set)
        }

        components match {
            case x :: xs => x match {
                case Star(Circle(left, right)) =>
                    (left, right) match {
                        case (Input(id1), Input(id2)) => componentsForAlgebra(xs, newAccDouble(id1, id2), counter + 1)
                        case (Input(id1), Output(id2)) => componentsForAlgebra(xs, newAccDouble(id1, id2), counter + 1)
                        case (Output(id1), Input(id2)) => componentsForAlgebra(xs, newAccDouble(id1, id2), counter + 1)
                        case (Output(id1), Output(id2)) => componentsForAlgebra(xs, newAccDouble(id1, id2), counter + 1)
                        case _ => throw new IllegalStateException
                    }
                case Star(Input(id)) => componentsForAlgebra(xs, newAccStar(id), counter + 1)
                case Star(Output(id)) => componentsForAlgebra(xs, newAccStar(id), counter + 1)
                case Input(id) => componentsForAlgebra(xs, newAccSingle(id), counter)
                case Output(id) => componentsForAlgebra(xs, newAccSingle(id), counter)
            }
            case Nil => acc
        }
    }

    def componentsForAlgebra(components: List[Expression]): Map[String, Set[String]] =
        componentsForAlgebra(components, Map.empty[String, Set[String]], 1)


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
