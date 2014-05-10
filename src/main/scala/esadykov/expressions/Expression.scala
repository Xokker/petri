package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 03.05.2014
 */
abstract class Expression {
    def normalize: Expression = this

    def atomic: Boolean = false

    // TODO: works incorrectly
    def components(): Array[Map[String, Component]] = {
        def extract(exp: Expression, insideStar: Boolean, acc: Map[String, Component]): Map[String, Component] = {
            def atomic(id: String): Map[String, Component] = {
                val comp = acc.getOrElse(id, null)
                if (comp != null)
                    if (insideStar)
                        acc + (id -> acc(id).incrementStars)
                    else
                        acc + (id -> acc(id).incrementWithoutStars)
                else
                    acc + (id -> new Component(id, 0, 1))
            }

            exp match {
                case Input(id) => atomic(id)
                case Output(id) => atomic(id)
                case Star(el) => extract(el, insideStar = true, acc)
                case Circle(left, right) => extract(right, insideStar, extract(left, insideStar, acc))
            }
        }
        this match {
            case Oplus(left, right) => left.components ++ right.components
            case Star(exp) => Array(extract(exp, insideStar = true, Map.empty))
            case Circle(left, right) => Array(extract(right, insideStar = false, extract(left, insideStar = false, Map.empty)))
            case Input(id) => Array(Map(id -> new Component(id, 0, 1)))
            case Output(id) => Array(Map(id -> new Component(id, 0, 1)))
        }
    }
}

object Expression {
    def main(args: Array[String]) {
        val expression: Expression = Circle(Star(Circle(Output("CD"), Input("CR"))),
            Oplus(Circle(Star(Input("A")), Output("E")), Circle(Output("CD"), Output("E"))))

        println(expression)

        val normalized: Expression = expression.normalize
        println(normalized)

        println(normalized.components().mkString("\n"))
    }
}
