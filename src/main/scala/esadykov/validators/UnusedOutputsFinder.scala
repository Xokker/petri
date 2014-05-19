package esadykov.validators

import esadykov.WorkflowNet
import esadykov.nets.NetNode

/**
 * @author Ernest Sadykov
 * @since 19.05.2014
 */
object UnusedOutputsFinder {
    def find(nets: Set[WorkflowNet]) = {
        var result: Set[NetNode] = Set.empty
        for (net <- nets) {
            val outputs: Set[NetNode] = net.outputs
            val inputs = (nets - net).flatMap(_.inputs)
            for {out <- outputs
                 if inputs.find(_.name == out.name).isEmpty} {
                result = result + out
            }
        }
        result
    }
}
