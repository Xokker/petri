package esadykov.commands

import esadykov.WorkflowNet

/**
 * @author Ernest Sadykov
 * @since 22.05.2014
 */
class ConnectionResult(val success: Boolean,
                       val updatedNets: List[WorkflowNet],
                       val error: String = "") {
    require(success || !error.isEmpty)

    val failed: Boolean = !success
}
