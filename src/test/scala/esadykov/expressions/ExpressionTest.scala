package esadykov.expressions

import org.scalatest.FunSuite

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
class ExpressionTest extends FunSuite {
    test("normalization simple") {
        val expression: Expression = Circle(Star(Circle(Output("CD"),
                                                        Input("CR"))),
                                                        Oplus(Circle(Star(Input("A")),
                                                            Output("E")),
                                                            Circle(Output("CD"),
                                                                Output("E"))))

        val normalized: Expression = expression.normalize()
        assert("((!CD◦?CR)*◦?A*◦!E)⊕((!CD◦?CR)*◦!CD◦!E)" === normalized.toString(noParen = true))
    }

    test("normalization complicated") {
        val expression: Expression = Circle(Circle(Output("CD"),
            Circle(Input("CR"), Star(Circle(Output("CD"),
                        Input("CR"))))),
                        Oplus(Circle(Circle(Input("A"), Star(Input("A"))),
                            Output("E")),
                            Circle(Output("CD"),
                                Output("E"))))

        val normalized: Expression = expression.normalize().normalize()
        assert("(!CD◦?CR◦(!CD◦?CR)*◦?A◦?A*◦!E)⊕(!CD◦?CR◦(!CD◦?CR)*◦!CD◦!E)" === normalized.toString(noParen = true))
    }
}
