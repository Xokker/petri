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

    /**
     * @param nets all nets that program works with
     * @return connection result
     */
    def connect(nets: IndexedSeq[WorkflowNet]): ConnectionResult = {
        if (nets.size < indexSocketPairs.size) {
            throw new IllegalArgumentException
        }

        var netsToWorkWith: List[WorkflowNet] = Nil
        for (pair <- indexSocketPairs) {
            if (pair._1 > nets.size) {
                return new ConnectionResult(false, nets, "There is no net with index " + pair._1)
            }
            netsToWorkWith = netsToWorkWith :+ nets(pair._1 - 1)
        }
        val lst: List[Set[String]] = indexSocketPairs.map(_._2).toList
        val connectionResult: String = tryConnect(netsToWorkWith, lst)

        if (connectionResult.isEmpty) {
            val newNets: IndexedSeq[WorkflowNet] =
                for (i <- 0 until nets.size) yield {
                    if (indexSocketPairs.contains(i + 1)) {
                        nets(i).netWithoutSockets(indexSocketPairs(i + 1))
                    } else {
                        nets(i)
                    }
                }
            new ConnectionResult(true, newNets)
        } else {
            new ConnectionResult(false, nets, connectionResult)
        }
    }

    /**
     * @return empty string if everything is fine, otherwise reason of the failure
     */
    private def tryConnect(nets: List[WorkflowNet], sockets: List[Set[String]]): String = {
        def findInput(): Int = {
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
        assert(nets.size == sockets.size)

        val inputIndex: Int = findInput()
        if (inputIndex == -2) return "Multiple input sockets forbidden"
        else if (inputIndex == -1) return "No input socket"

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
                return "Net " + (i + 1) + " is not compatible with net " + (inputIndex + 1)
            }
        }

        ""
    }
}


