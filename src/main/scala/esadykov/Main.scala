package esadykov

import esadykov.nets.NetNode
import esadykov.expressions.Expression
import esadykov.math.ConsistencyChecker

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object Main {
    private[this] val UsageString = "Usage: ./petri.jar <first_net> <second_net> [<third_net> ... <nth_net>]"

    def main(args: Array[String]) {
        if (args.length < 2) {
            println(UsageString)
            sys.exit(-1)
        }

        println(args(0) + " " + args(1))

        val firstNet: WorkflowNet = WorkflowNet.createFromFile(args(0))
        val secondNet: WorkflowNet = WorkflowNet.createFromFile(args(1))

        val componentsForAlgebra1: List[Map[String, List[String]]] = firstNet.componentsForAlgebra()
        val componentsForAlgebra2: List[Map[String, List[String]]] = secondNet.componentsForAlgebra(100)

        println("components 1: \n" + componentsForAlgebra1.mkString("\n"))
        println("components 2: \n" + componentsForAlgebra2.mkString("\n"))

        println(ConsistencyChecker.check(componentsForAlgebra1, componentsForAlgebra2))

    }
}
