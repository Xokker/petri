package esadykov.commands

import esadykov.WorkflowNet
import org.apache.commons.lang3.RandomStringUtils
import esadykov.math.ConsistencyChecker._
import esadykov.math.ConsistencyChecker
import esadykov.nets.NetNode

/**
 * @author Ernest Sadykov
 * @since 21.05.2014
 */
class NetsConnector(indexSocketPairs: Map[Int, Set[String]]) extends Command {

    def findInput(nets: IndexedSeq[WorkflowNet], sockets: List[Set[String]]): Int = {
        assert(nets.size == sockets.size)
        var res = -1
        for {i <- 0 until nets.length
             s <- sockets(i)
             if nets(i).sockets.exists(node => node.name == s && node.input)} {
            {
                if (res != -1 || sockets(i).size != 1) return -2
                else res = i
            }
        }
        res
    }

    /**
     * @param nets all nets that program works with
     * @return connection result
     */
    def connect(nets: IndexedSeq[WorkflowNet]): ConnectionResult = {
        if (nets.size < indexSocketPairs.size) {
            throw new IllegalArgumentException
        }

        var netsToWorkWith: IndexedSeq[WorkflowNet] = IndexedSeq.empty
        for (pair <- indexSocketPairs) {
            if (pair._1 > nets.size) {
                return new ConnectionResult(false, nets, "There is no net with index " + pair._1)
            }
            netsToWorkWith = netsToWorkWith :+ nets(pair._1 - 1)
        }
        val lst: List[Set[String]] = indexSocketPairs.map(_._2).toList

        val inputIndex: Int = findInput(netsToWorkWith, lst)
        if (inputIndex == -2) return new ConnectionResult(false, nets, "Multiple input sockets forbidden")
        else if (inputIndex == -1) return new ConnectionResult(false, nets, "No input socket")

        val connectionResult: String = tryConnect(netsToWorkWith, lst, inputIndex)

        if (connectionResult.isEmpty) {
            val newNets: IndexedSeq[WorkflowNet] =
                for (i <- 0 until nets.size) yield {
                    if (indexSocketPairs.contains(i + 1)) nets(i).netWithoutSockets(indexSocketPairs(i + 1))
                    else nets(i)
                }

            val inputSocket: NetNode = netsToWorkWith(inputIndex).sockets.find(_.name == lst(inputIndex).head).get
            val outSockets: IndexedSeq[NetNode] =
                for {i <- 0 until netsToWorkWith.size
                     if i != inputIndex
                     s <- netsToWorkWith(i).sockets
                     if lst(i).contains(s.name)} yield s

            new ConnectionResult(true, newNets, outputSockets = outSockets, inputSocket = inputSocket)
        } else {
            new ConnectionResult(false, nets, connectionResult)
        }
    }

    /**
     * @return empty string if everything is fine, otherwise reason of the failure
     */
    private def tryConnect(nets: IndexedSeq[WorkflowNet], sockets: List[Set[String]], inputIndex: Int): String = {

        val newId = RandomStringUtils.randomAlphabetic(3)

        var newComponents: List[AlgebraComponents] = Nil
        for {i <- 0 until nets.length
             currentNet = nets(i)
        } {
            var newComp: AlgebraComponents = Nil
            for {c <- currentNet.componentsForAlgebra
                 appropriate = c.find(pair => sockets(i).contains(pair._1))
                 if appropriate.isDefined
            } {
                newComp = newComp :+ Map(newId -> appropriate.get._2)
            }
            newComponents = newComponents :+ newComp
        }

        for {i <- 0 until nets.length
             if i != inputIndex
        } {
            if (!ConsistencyChecker.check(newComponents(i), newComponents(inputIndex))) {
                return "Sockets of the net " + (i + 1) + " is not compatible with sockets of the net " + (inputIndex + 1)
            }
        }

        ""
    }
}


