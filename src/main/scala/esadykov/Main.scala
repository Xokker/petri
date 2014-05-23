package esadykov

import scala.annotation.tailrec
import esadykov.commands._
import esadykov.nets.NetNode

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

        val nets: IndexedSeq[WorkflowNet] = args.map(WorkflowNet.createFromFile)

        println("Type 'help' to get usage instructions")
        printNetsInfo(nets)

        val modelMap: Map[String, (Int, Int, Int, Int)] = nets.flatMap(_.model)
            .foldLeft(Map.empty[String, (Int, Int, Int, Int)])((m, pair) => m + (pair._1 -> pair._2))

        interactiveMode(nets, modelMap)
    }

    private def printNetsInfo(nets: Seq[WorkflowNet]) {
        println("Read " + nets.size + " nets: " + nets.map(_.name).mkString(", "))
        println("Generated expressions:")
        var counter = 0
        for (n <- nets) {
            println((counter + 1) + ". [" + n.name + "] " + n.expression)
            counter = counter + 1
        }
        println()
    }

    /**
     * Interaction with user.
     * Method assumes that nets are open and syntactic compatible.
     */
    private def interactiveMode(nets0: IndexedSeq[WorkflowNet], modelData: Map[String, (Int, Int, Int, Int)]) {
        def availableSocketsString(net: WorkflowNet): String =
            "Available sockets: " + net.sockets
                .map(node => if (node.input) "?" + node.name else "!" + node.name)
                .mkString(" ")

        @tailrec
        def mainLoop(nets: IndexedSeq[WorkflowNet], outputSockets: List[IndexedSeq[NetNode]], inputSocket: List[NetNode], message: String = "") {
            if (!message.isEmpty) println(message + "\n")
            var counter = 1
            for (n <- nets) {
                println(counter + ". [" + n.name + "] " + availableSocketsString(n))
                counter = counter + 1
            }
            val read = readLine(">")
            if (read == null) return // Ctrl-C handle
            val command: Command = InputParser.parseInput(read)
            command match {
                case ExitCommand => ()
                case con: NetsConnector =>
                    val connectionResult: ConnectionResult = con.connect(nets)
                    if (connectionResult.failed) {
                        mainLoop(nets, outputSockets, inputSocket, connectionResult.error)
                    } else {
                        mainLoop(connectionResult.updatedNets, outputSockets :+ connectionResult.outputSockets,
                            inputSocket :+ connectionResult.inputSocket, "Connection successful")
                    }
                case hold: ErrorHolder => mainLoop(nets, outputSockets, inputSocket, hold.message)
                case EmptyCommand => mainLoop(nets, outputSockets, inputSocket)
                case HelpCommand => mainLoop(nets, outputSockets, inputSocket, HelpCommand.HelpPrompt)
                case build: BuildAdaptersCommand =>
                    val error =
                    try {
                        build.buildAdapters(modelData, outputSockets.toIndexedSeq, inputSocket.toIndexedSeq)
                        ""
                    } catch {
                        case e: Exception => "Error saving file " + build.path
                    }
                    mainLoop(nets, outputSockets, inputSocket, error)
            }
        }

        mainLoop(nets0, Nil, Nil)
    }
}

