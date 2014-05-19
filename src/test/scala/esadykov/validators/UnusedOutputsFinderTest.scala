package esadykov.validators

import org.scalatest.FunSuite
import esadykov.WorkflowNet
import esadykov.nets.NetNode

/**
 * @author Ernest Sadykov
 * @since 19.05.2014
 */
class UnusedOutputsFinderTest extends FunSuite {
    test("test 1") {
        val testFiles = List("My0011.npnets", "My0012.npnets", "My0013.npnets")
        val nets: List[WorkflowNet] = testFiles.map(WorkflowNet.createFromFile)

        val result: Array[NetNode] = UnusedOutputsFinder.find(nets.toSet).toArray
        assert(result.size === 1)
        assert(result(0).name === "Unused")
    }
}
