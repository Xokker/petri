package esadykov

import esadykov.nets.{NetElement, NetNode}

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object Main {
    def main(args: Array[String]) {
        val res: Map[String, NetElement] = XmlNetManager.readNetElements("bank_net.xml")
        println(res.mkString("\n"))
        XmlNetManager.connectNodes(res)

        val source = NetNode.findSource(res.values)

        println(source)
    }
}
