package esadykov

import esadykov.math.ConsistencyChecker.AlgebraComponents
import scala.util.matching.Regex
import scala.annotation.tailrec
import org.apache.commons.lang3.RandomStringUtils
import esadykov.math.ConsistencyChecker

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 *
 * TODO: add checking that sockets in each WFM are unique
 */
object Main {
    private[this] val UsageString = "Usage: ./petri.jar <first_net> <second_net> [<third_net> ... <nth_net>]"

    def main(args: Array[String]) {
        if (args.length < 2) {
            println(UsageString)
            sys.exit(-1)
        }

        val nets: List[WorkflowNet] = args.map(WorkflowNet.createFromFile).toList

        interactiveMode(nets)
    }

    private[this] val Prompt =
        """
          |type 'exit' for quit the application
          |type 'connect 1.A 3.Dc' for connect socket A of the net 1 with socket Dc of the net 3
        """.stripMargin

    /**
     * Interaction with user.
     * Method assumes that nets are open and syntactic compatible.
     */
    private def interactiveMode(nets0: List[WorkflowNet]) {
        def availableSocketsString(net: WorkflowNet): String =
            "Available sockets: " + net.sockets
                .map(node => if (node.input) "?"+node.name else "!"+node.name)
                .mkString(" ")

        @tailrec
        def mainLoop(nets: List[WorkflowNet], message: String = "") {
            println(if (message.isEmpty) Prompt else message + "\n" + Prompt)
            var counter = 1
            for (n <- nets) {
                println(counter + ". " + availableSocketsString(n))
                counter = counter + 1
            }
            val read = readLine()
            val Pattern: Regex = "connect(.*)".r
            read match {
                case Pattern(g) =>
                    val args: Array[String] = g.split(" ").filter(_ != "")
                    if (args.length != 2) mainLoop(nets, "Wrong number of arguments\n")
                    else {
                        val mapRes: Array[Array[String]] = args.map(_.split("\\."))
                        val firstNetIndex: Int = mapRes(0)(0).toInt - 1
                        val secondNetIndex: Int = mapRes(1)(0).toInt - 1
                        val firstSocket: String = mapRes(0)(1)
                        val secondSocket: String = mapRes(1)(1)
                        val connectionResult: Boolean = tryConnect(nets(firstNetIndex), nets(secondNetIndex), Set(firstSocket), Set(secondSocket))
                        var newNets: List[WorkflowNet] = Nil
                        if (connectionResult) {
                            var counter = 0
                            for (n <- nets) {
                                if (counter == firstNetIndex) {
                                    newNets = newNets :+ nets(firstNetIndex).netWithoutSockets(Set(firstSocket))
                                } else if (counter == secondNetIndex) {
                                    newNets = newNets :+ nets(secondNetIndex).netWithoutSockets(Set(secondSocket))
                                } else {
                                    newNets = newNets :+ n
                                }
                                counter = counter + 1
                            }
                        } else {
                            newNets = nets
                        }

                        if (connectionResult) mainLoop(newNets, "Connected!")
                        else mainLoop(nets, "Not connected")
                    }
                case "exit" => ()
                case _ => mainLoop(nets)
            }
        }

        mainLoop(nets0)
    }

    def tryConnect(net1: WorkflowNet, net2: WorkflowNet,
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

