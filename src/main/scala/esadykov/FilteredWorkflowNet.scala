package esadykov

import esadykov.nets.NetNode
import esadykov.expressions.{Input, Output, Expression}

/**
 * @author Ernest Sadykov
 * @since 20.05.2014
 */
class FilteredWorkflowNet(source: NetNode, sink:NetNode, sockets: Set[NetNode], filterOut: Set[String], name: String, model: Map[String, (Int, Int, Int, Int)])
    extends WorkflowNet(source, sink, sockets, name, model) {

    override def netWithoutSockets(socketsOut: Set[String]): WorkflowNet =
        new FilteredWorkflowNet(source, sink,
                                sockets.filterNot(n => socketsOut.contains(n.name)),
                                socketsOut ++ filterOut, name, model)

    override protected def findSockets(node: NetNode): Set[Expression] =
        (node.connections
            .filter(n => n.socket && !filterOut.contains(n.name))
            .foldLeft(Set.empty[Expression])((s, n) => s + new Output(n.name))
            ++
            sockets.filter(_.connections.contains(node))
                .foldLeft(Set.empty[Expression])((s, n) => s + new Input(n.name)))
}

object FilteredWorkflowNet {
    def main(args: Array[String]) {
        val workflowNet: WorkflowNet = WorkflowNet.createFromFile("/Users/ernest/eclipse/runtime-EclipseApplication/nets/My11.npnets")
        println(workflowNet.componentsForAlgebra)
        println(workflowNet.expression)
        println(workflowNet.sockets)

        val workflowNetFiltered: WorkflowNet = workflowNet.netWithoutSockets(Set("A", "CD"))
        println(workflowNetFiltered.componentsForAlgebra)
        println(workflowNetFiltered.expression)
        println(workflowNetFiltered.sockets)


        val workflowNetFiltered2: WorkflowNet = workflowNetFiltered.netWithoutSockets(Set("CR"))
        println(workflowNetFiltered2.componentsForAlgebra)
        println(workflowNetFiltered2.expression)
        println(workflowNetFiltered2.sockets)
    }
}
