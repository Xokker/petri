package esadykov.nets

import scala.collection.Iterable

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetNode(uuid: String, val name: String) extends NetElement(uuid) {
    val input: Boolean = name.startsWith("I:")
    val output: Boolean = name.startsWith("O:")
    val source: Boolean = name == "source"
    val sink: Boolean = name == "sink"
    var connections: Array[NetNode] = Array.empty[NetNode]

    def connectWith(anotherNode: NetNode) =
        connections = connections :+ anotherNode

    override def toString =
        getClass.getSimpleName+"[uuid="+uuid+",name="+name+"," +
            "input="+input+",output="+output+"]"
}

object NetNode {
    def findSource(elements: Iterable[NetElement]): NetNode =
        elements
            .find(el => el.isInstanceOf[NetNode] && el.asInstanceOf[NetNode].source)
            .getOrElse(throw new IllegalArgumentException("No source in elements"))
            .asInstanceOf[NetNode]
}
