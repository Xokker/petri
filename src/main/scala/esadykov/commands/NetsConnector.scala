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
    // TODO: implement
    private def checkIOBalance(): String = {
        ""
    }

    def connect(nets: List[WorkflowNet]): ConnectionResult = {
        if (nets.size < indexSocketPairs.size) {
            throw new IllegalArgumentException
        }

        val iterator: Iterator[(Int, Set[String])] = indexSocketPairs.iterator
        val firstPair: (Int, Set[String]) = iterator.next()
        val secondPair: (Int, Set[String]) = iterator.next()

        val firstNet: WorkflowNet = {
            if (firstPair._1 > nets.size)
                return new ConnectionResult(false, nets, "Index " + firstPair._1 + " is out")
            nets(firstPair._1 - 1)
        }
        val secondNet: WorkflowNet = {
            if (secondPair._1 > nets.size)
                return new ConnectionResult(false, nets, "Index " + secondPair._1 + " is out")
            nets(secondPair._1 - 1)
        }

        val ioBalance: String = checkIOBalance()
        if (!ioBalance.isEmpty) return new ConnectionResult(false, nets, ioBalance)

        val connectionResult: Boolean = tryConnect(firstNet, secondNet, firstPair._2, secondPair._2)
        var newNets: List[WorkflowNet] = Nil
        if (connectionResult) {
            var counter = 0
            for (n <- nets) {
                if (n == firstNet) {
                    newNets = newNets :+ firstNet.netWithoutSockets(firstPair._2)
                } else if (n == secondNet) {
                    newNets = newNets :+ secondNet.netWithoutSockets(secondPair._2)
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


