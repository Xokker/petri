package esadykov

import esadykov.nets.{NetElement, NetNode}
import esadykov.expressions.Empty

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object Main {
    def main(args: Array[String]) {
        val res: Map[String, NetElement] = XmlNetManager.readNetElements("bank_net.xml")
        println(res.mkString("\n"))
        XmlNetManager.connectNodes(res)

        val source = NetNode.findSource(res.values)

        println(source)

        val generatedExpression = NetNode.traverse(
            start = source,
            wereThere = Set.empty,
            acc = Empty(),
            inputs = res.values
                .filter(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].input)
                .map(_.asInstanceOf[NetNode])
                .toSet,
            destination = NetNode.findSink(res.values)
        )
        println(generatedExpression.normalize)
    }
}
