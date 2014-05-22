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
class WorkflowNet(val source: NetNode, val sink: NetNode, val sockets: Set[NetNode],
                  val name: String = "", val model: Map[String, (Int, Int, Int, Int)] = Map.empty) {

    def expression: Expression = {
        val expr: Expression = traverse(
            start = source,
            wereThere = Set.empty,
            acc = Empty(),
            destination = sink
        )
        expr.normalize().normalize().normalize().normalize().normalize()
    }

    lazy val componentsForAlgebra: AlgebraComponents = {
        val components: List[List[Expression]] = Expression.components(expression)

        components.map(Expression.componentsForAlgebra(_))
    }

    protected def findSockets(node: NetNode): Set[Expression] =
        (node.connections
            .filter(_.socket)
            .foldLeft(Set.empty[Expression])((s, n) => s + new Output(n.name))
            ++
            sockets.filter(_.connections.contains(node))
                .foldLeft(Set.empty[Expression])((s, n) => s + new Input(n.name)))

    private def traverse(start: NetNode, wereThere: Set[NetNode], acc: Expression, destination: NetNode): Expression = {
        if (start == destination) acc
        else if (wereThere.contains(start)) Empty() // loop
        else {
            val IO: Set[Expression] = findSockets(start)
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

    def netWithoutSockets(socketsOut: Set[String]): WorkflowNet =
        new FilteredWorkflowNet(source, sink, sockets.filterNot(n => socketsOut.contains(n.name)), socketsOut, name, model)

    def canEqual(other: Any): Boolean = other.isInstanceOf[WorkflowNet]

    override def equals(other: Any): Boolean = other match {
        case that: WorkflowNet =>
            (that canEqual this) &&
                source == that.source &&
                sink == that.sink &&
                sockets.size == that.sockets.size
        case _ => false
    }

    override def hashCode(): Int = {
        val state = Seq(source, sink, sockets.size)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }
}

object WorkflowNet {

    private def find(elements: Iterable[NetElement])(errorMessage: String, p: NetNode => Boolean) =
        elements
            .find(el => el.isInstanceOf[NetNode] && p(el.asInstanceOf[NetNode]))
            .getOrElse(throw new IllegalArgumentException(errorMessage))
            .asInstanceOf[NetNode]

    def findSource(elements: Iterable[NetElement]): NetNode =
        find(elements)("No source in elements", _.source)

    def findSink(elements: Iterable[NetElement]): NetNode =
        find(elements)("No sink in elements", _.sink)

    private def sockets(netElements1: Map[String, NetElement]): Set[NetNode] =
        netElements1.values
            .filter(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].socket)
            .map(_.asInstanceOf[NetNode])
            .toSet

    private[this] val DefaulName = "Untitled SN"
    def createFromFile(filename: String): WorkflowNet = {
        val (elements, name, modelInfo) = XmlNetManager.readNetElements(filename)
        XmlNetManager.connectNodes(elements)
        new WorkflowNet(findSource(elements.values), findSink(elements.values), sockets(elements), name.trim, modelInfo)
    }
}
