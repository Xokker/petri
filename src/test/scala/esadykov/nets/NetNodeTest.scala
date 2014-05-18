package esadykov.nets

import org.scalatest.FunSuite

/**
 * @author Ernest Sadykov
 * @since 10.05.2014
 */
class NetNodeTest extends FunSuite {
    test("expression generation") {
//        val res: Map[String, NetElement] = XmlNetManager.readNetElements("bank_net.xml")
//        XmlNetManager.connectNodes(res)
//        val source = NetNode.findSource(res.values)
//
//        val generatedExpression = NetNode.traverse(
//            start = source,
//            wereThere = Set.empty,
//            acc = Empty(),
//            inputs = res.values
//                .filter(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].input)
//                .map(_.asInstanceOf[NetNode])
//                .toSet,
//            destination = NetNode.findSink(res.values)
//        )
//        val normalized: Expression = generatedExpression.normalize().normalize()
//        val expected: Expression = Circle(Circle(Output("CD"),
//            Circle(Input("CR"), Star(Circle(Output("CD"),
//                Input("CR"))))),
//            Oplus(Circle(Circle(Input("A"), Star(Input("A"))),
//                Output("E")),
//                Circle(Output("CD"),
//                    Output("E"))))
//        val expectedNormalized: Expression = expected.normalize().normalize()
//
//        assert(normalized.toString(noParen = false) === expectedNormalized.toString(noParen = false))
    }
}
