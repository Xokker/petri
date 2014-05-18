package esadykov

import esadykov.nets.{NetElement, NetNode}
import scala.collection.Iterable
import esadykov.expressions._
import esadykov.expressions.Input
import esadykov.expressions.Empty
import esadykov.expressions.Star
import esadykov.expressions.Output
import esadykov.expressions.Circle
import esadykov.math.ConsistencyChecker.AlgebraComponents

/**
 * @author Ernest Sadykov
 * @since 18.05.2014
 */
class WorkflowNet(val source: NetNode, val sink: NetNode, val inputs: Set[NetNode]) {
    lazy val outputs: Set[NetNode] = {
        def traverse(start: NetNode, visited: Set[NetNode]): Set[NetNode] =
            if (visited.contains(start)) Set.empty
            else start.connections.filter(_.output).toSet ++ start.connections.flatMap(node => traverse(node, visited + start))
        traverse(source, Set.empty)
    }

    def traverse(start: NetNode, wereThere: Set[NetNode], acc: Expression, destination: NetNode): Expression = {
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
                traverse(nonOutput.head, wereThere + start, newExpression, destination)
            } else {
                val loopCandidates: Array[Expression] =
                    nonOutput
                        .map(traverse(_, Set.empty, Empty(), start).normalize())
                val newNewExpression = Circle(newExpression, loopCandidates.foldLeft[Expression](Empty()) {
                    (ex, el) => Circle(ex, Star(el))
                })

                nonOutput
                    .map(node => traverse(node, wereThere + start, newNewExpression, destination))
                    .foldLeft[Expression](Empty()) {
                    (exp, el) => Oplus(exp, el)
                }
            }
        }
    }

    def componentsForAlgebra(counter: Int = 1): AlgebraComponents = {
        val generatedExpression = traverse(
            start = source,
            wereThere = Set.empty,
            acc = Empty(),
            destination = sink
        )
        val normalized: Expression = generatedExpression.normalize().normalize()
        val components: List[List[Expression]] = Expression.components(normalized)

        components.map(Expression.componentsForAlgebra(_, counter))
    }
}

object WorkflowNet {
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

    private def inputs(netElements1: Map[String, NetElement]): Set[NetNode] =
        netElements1.values
            .filter(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].input)
            .map(_.asInstanceOf[NetNode])
            .toSet

    def createFromFile(filename: String): WorkflowNet = {
        val elements: Map[String, NetElement] = XmlNetManager.readNetElements(filename)
        XmlNetManager.connectNodes(elements)
        new WorkflowNet(findSource(elements.values), findSink(elements.values), inputs(elements))
    }
}
