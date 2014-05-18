package esadykov.nets

import scala.collection.Iterable
import esadykov.expressions._

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetNode(uuid: String, _name: String) extends NetElement(uuid) {
    private[this] val InputPrefix = "I:"
    private[this] val OutputPrefix = "O:"

    val input: Boolean = _name.startsWith(InputPrefix)
    val output: Boolean = _name.startsWith(OutputPrefix)
    val source: Boolean = _name == "source"
    val sink: Boolean = _name == "sink"
    var connections: Array[NetNode] = Array.empty[NetNode]

    def connectWith(anotherNode: NetNode): Unit =
        connections = connections :+ anotherNode

    def name: String =
        _name.stripPrefix(InputPrefix).stripPrefix(OutputPrefix)

    override def toString =
        getClass.getSimpleName+"[uuid="+uuid+",name="+name+"," +
            "input="+input+",output="+output+"]"
}

object NetNode {
    def findSource(elements: Iterable[NetElement]): NetNode =
        elements
            .find(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].source)
            .getOrElse(throw new IllegalArgumentException("No source in elements"))
            .asInstanceOf[NetNode]

    // TODO: eliminate code duplication
    def findSink(elements: Iterable[NetElement]): NetNode =
        elements
            .find(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].sink)
            .getOrElse(throw new IllegalArgumentException("No sink in elements"))
            .asInstanceOf[NetNode]

    def inputs(netElements1: Map[String, NetElement]): Set[NetNode] =
        netElements1.values
            .filter(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].input)
            .map(_.asInstanceOf[NetNode])
            .toSet

    def traverse(start: NetNode, wereThere: Set[NetNode], acc: Expression, inputs: Set[NetNode], destination: NetNode): Expression = {
        def findInputsAndOutputs(node: NetNode): Set[Expression] =
            node.connections
                .filter(_.output)
                .foldLeft(Set.empty[Expression])((s, n) => s + new Output(n.name)) ++
                inputs.filter(_.connections.contains(node))
                    .foldLeft(Set.empty[Expression])((s, n) => s + new Input(n.name))

        if (start == destination) acc
        else if (wereThere.contains(start)) Empty() // loop
        else {
            val IO: Set[Expression] = findInputsAndOutputs(start)
            val nonOutput = start.connections.filter(!_.output)
            val newExpression: Expression = IO.foldLeft(acc)((ex, el) => Circle(ex, el))

            if (nonOutput.length == 1) {
                traverse(nonOutput.head, wereThere + start, newExpression, inputs, destination)
            } else {
                val loopCandidates: Array[Expression] =
                    nonOutput
                        .map(traverse(_, Set.empty, Empty(), inputs, start).normalize())
                val newNewExpression = Circle(newExpression, loopCandidates.foldLeft[Expression](Empty()) {
                    (ex, el) => Circle(ex, Star(el))
                })

                nonOutput
                    .map(node => traverse(node, wereThere + start, newNewExpression, inputs, destination))
                    .foldLeft[Expression](Empty()) {
                        (exp, el) => Oplus(exp, el)
                    }
            }
        }
    }
}
