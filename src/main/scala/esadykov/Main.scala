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
        """type 'exit' for quit the application
          |type 'connect 1.A 3.Dc' for connect socket A of the net 1 with socket Dc of the net 3
        """.stripMargin

    /**
     * Interaction with user.
     * Method assumes that nets are open and syntactic compatible.
     */
    private def interactiveMode(nets: List[WorkflowNet]) {
        def availableSocketsString(net: WorkflowNet): String =
            "Available sockets: " + net.sockets.map(_.name).mkString(" ")

        @tailrec
        def loop(message: String = "") {
            println(if (message.isEmpty) Prompt else message + Prompt)
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
                    if (args.length != 2) loop("Wrong number of arguments\n")
                    else {
                        val connection: ((Int, String), (Int, String)) = {
                            val mapRes: Array[Array[String]] = args.map(_.split("\\."))
                            ((mapRes(0)(0).toInt, mapRes(0)(1)), (mapRes(1)(0).toInt, mapRes(1)(1)))
                        }
                        println(connection + " is " + tryConnect(nets(connection._1._1 - 1), nets(connection._2._1 - 1),
                            Set(connection._1._2), Set(connection._2._2)))
                        loop()
                    }
                case "exit" => ()
                case _ => loop()
            }
        }

        val components: List[AlgebraComponents] = nets.map(_.componentsForAlgebra)
//        println("components: \n" + components.mkString("\n"))

        loop()
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

