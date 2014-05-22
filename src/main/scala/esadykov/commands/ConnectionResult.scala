package esadykov.commands

import esadykov.WorkflowNet
import esadykov.nets.NetNode

/**
 * @author Ernest Sadykov
 * @since 22.05.2014
 */
class ConnectionResult(val success: Boolean,
                       val updatedNets: IndexedSeq[WorkflowNet],
                       val error: String = "",
                       val outputSockets: Set[NetNode] = Set.empty,
                       val inputSocket: NetNode = null) {
    require(success || !error.isEmpty)

    val failed: Boolean = !success
}
