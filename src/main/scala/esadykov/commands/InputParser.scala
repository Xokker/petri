package esadykov.commands

import scala.util.matching.Regex
import scala.collection.immutable.ListMap

/**
 * @author Ernest Sadykov
 * @since 21.05.2014
 */
object InputParser {

    private def parseConnect(connectArgs: String): Command = {
        val args: Array[String] = connectArgs.split(" ").filter(_ != "")
        if (args.length < 2) {
            new ErrorHolder("Wrong number of arguments (should be at least 2 nets)\n")
        } else {
            val mapRes: Array[Array[String]] = args.map(_.split("\\."))
            var connections: ListMap[Int, Set[String]] = ListMap.empty

            for (strArray <- mapRes) {
                if (strArray.length != 2) {
                    return new ErrorHolder("Wrong argument '" + strArray.mkString + "'")
                }
                val sockets: Set[String] = strArray(1).split('&').filterNot(_ == "").toSet
                val netIndex = try {
                    strArray(0).toInt
                } catch {
                    case e: NumberFormatException => return new ErrorHolder("There is no petri net number " + strArray(0))
                }
                if (connections.contains(netIndex)) {
                    return new ErrorHolder("Multiple arguments with same index")
                }
                connections = connections + (netIndex -> sockets)
            }
            new NetsConnector(connections)
        }
    }

    def parseInput(input: String): Command = {
        val Pattern: Regex = "connect(.*)".r
        input.trim() match {
            case Pattern(g) => parseConnect(g)
            case "exit" => ExitCommand
            case "help" => HelpCommand
            case "" => EmptyCommand
            case _ => new ErrorHolder("Unknown command")
        }
    }
}
