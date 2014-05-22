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
    private def checkIOBalance(nets: List[WorkflowNet]): String = {
        ""
    }

    /**
     *
     * @param nets all nets that program works with
     * @return connection result
     */
    def connect(nets: List[WorkflowNet]): ConnectionResult = {
        if (nets.size < indexSocketPairs.size) {
            throw new IllegalArgumentException
        }

        val iterator: Iterator[(Int, Set[String])] = indexSocketPairs.iterator
        val firstPair: (Int, Set[String]) = iterator.next()
        val secondPair: (Int, Set[String]) = iterator.next()

        val firstNet: WorkflowNet = {
            if (firstPair._1 > nets.size)
                return new ConnectionResult(false, nets, "There is no net with index " + firstPair._1)
            nets(firstPair._1 - 1)
        }
        val secondNet: WorkflowNet = {
            if (secondPair._1 > nets.size)
                return new ConnectionResult(false, nets, "There is no net with index " + secondPair._1)
            nets(secondPair._1 - 1)
        }

        val ioBalance: String = checkIOBalance(nets)
        if (!ioBalance.isEmpty) return new ConnectionResult(false, nets, ioBalance)

        val connectionResult: String = tryConnect(List(firstNet, secondNet), List(firstPair._2, secondPair._2))
        var newNets: List[WorkflowNet] = Nil
        if (connectionResult.isEmpty) {
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
            new ConnectionResult(false, nets, connectionResult)
        }
    }

    /**
     * @return empty string if everything is fine, otherwise reason of the failure
     */
    private def tryConnect(nets: List[WorkflowNet], sockets: List[Set[String]]): String = {
        def findInput(): Int = {
            for {i <- 0 until nets.length
                 s <- sockets(i)
                 if nets(i).sockets.exists(node => node.name == s && node.input)} {
                {
                    return i
                }
            }
            -1
        }
        assert(nets.size == sockets.size)

        val inputIndex: Int = findInput()
        assert(inputIndex != -1)

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


