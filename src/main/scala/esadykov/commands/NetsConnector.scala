package esadykov.commands

import esadykov.WorkflowNet
import org.apache.commons.lang3.RandomStringUtils
import esadykov.math.ConsistencyChecker._
import esadykov.math.ConsistencyChecker

/**
 * @author Ernest Sadykov
 * @since 21.05.2014
 */
class NetsConnector(indexSocketPairs: Map[Int, Set[String]]) extends Command {
    def connect(nets: List[WorkflowNet]): ConnectionResult = {
        if (nets.size < indexSocketPairs.size) {
            throw new IllegalArgumentException
        }

        val iterator: Iterator[(Int, Set[String])] = indexSocketPairs.iterator
        val firstPair: (Int, Set[String]) = iterator.next()
        val secondPair: (Int, Set[String]) = iterator.next()

        val firstNetIndex: Int = {
            if (firstPair._1 > nets.size)
                return new ConnectionResult(false, nets, "Index " + firstPair._1 + " is out")
            firstPair._1 - 1
        }
        val secondNetIndex: Int = {
            if (secondPair._1 > nets.size)
                return new ConnectionResult(false, nets, "Index " + secondPair._1 + " is out")
            secondPair._1 - 1
        }

        val connectionResult: Boolean = tryConnect(nets(firstNetIndex), nets(secondNetIndex), firstPair._2, secondPair._2)
        var newNets: List[WorkflowNet] = Nil
        if (connectionResult) {
            var counter = 0
            for (n <- nets) {
                if (counter == firstNetIndex) {
                    newNets = newNets :+ nets(firstNetIndex).netWithoutSockets(firstPair._2)
                } else if (counter == secondNetIndex) {
                    newNets = newNets :+ nets(secondNetIndex).netWithoutSockets(secondPair._2)
                } else {
                    newNets = newNets :+ n
                }
                counter = counter + 1
            }
            new ConnectionResult(true, newNets)
        } else {
            new ConnectionResult(false, nets, "Connection failed")
        }
    }

    private def tryConnect(net1: WorkflowNet, net2: WorkflowNet,
                           net1Sockets: Set[String], net2Sockets: Set[String]): Boolean = {
        val newId = RandomStringUtils.randomAlphabetic(3)

        var newComponents1: AlgebraComponents = Nil
        for {c <- net1.componentsForAlgebra
             appropriate = c.find(pair => net1Sockets.contains(pair._1))
             if appropriate.isDefined} {
            newComponents1 = newComponents1 :+ Map(newId -> appropriate.get._2)
        }

        var newComponents2: AlgebraComponents = Nil
        for {c <- net2.componentsForAlgebra
             appropriate = c.find(pair => net2Sockets.contains(pair._1))
             if appropriate.isDefined} {
            newComponents2 = newComponents2 :+ Map(newId -> appropriate.get._2)
        }

        ConsistencyChecker.check(newComponents1, newComponents2)
    }
}

class ConnectionResult(val success: Boolean,
                       val updatedNets: List[WorkflowNet],
                       val error: String = "") {
    require(success || !error.isEmpty)

    val failed: Boolean = !success
}
