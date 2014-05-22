package esadykov

import scala.annotation.tailrec
import esadykov.commands._

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

        println("Type 'help' to get usage instructions")
        printNetsInfo(nets)
        interactiveMode(nets)
    }

    private def printNetsInfo(nets: List[WorkflowNet]) {
        println("Read " + nets.size + " nets")
        println("Generated expressions:")
        var counter = 0
        for (n <- nets) {
            println(counter + ". " + n.expression)
            counter = counter + 1
        }
        println()
    }

    /**
     * Interaction with user.
     * Method assumes that nets are open and syntactic compatible.
     */
    private def interactiveMode(nets0: List[WorkflowNet]) {
        def availableSocketsString(net: WorkflowNet): String =
            "Available sockets: " + net.sockets
                .map(node => if (node.input) "?" + node.name else "!" + node.name)
                .mkString(" ")

        @tailrec
        def mainLoop(nets: List[WorkflowNet], message: String = "") {
            if (!message.isEmpty) println(message + "\n")
            var counter = 1
            for (n <- nets) {
                println(counter + ". " + availableSocketsString(n))
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
                        mainLoop(nets, connectionResult.error)
                    } else {
                        mainLoop(connectionResult.updatedNets, "Connection successful")
                    }
                case hold: ErrorHolder => mainLoop(nets, hold.message)
                case EmptyCommand => mainLoop(nets)
                case HelpCommand => mainLoop(nets, HelpCommand.HelpPrompt)
            }
        }

        mainLoop(nets0)
    }
}

