package esadykov
import scala.xml.XML._
import esadykov.nets.{NetElement, NetNode, NetArc}

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object XmlNetManager {
    def readNetElements(filename: String): Map[String, NetElement] = {
        val netSystem = ((load(getClass.getResourceAsStream(filename))
            \ "net") (0) \ "netSystem") (0)
        val elements = (netSystem \ "nodes").map(node => new NetNode((node \ "@uuid").text, (node \ "@name").text)) ++
            (netSystem \ "arcs").map(arc => new NetArc((arc \ "@uuid").text, (arc \ "@source").text.drop(1), (arc \ "@target").text.drop(1)))
        elements.map(elem => elem.uuid -> elem)
            .foldLeft(Map.empty[String, NetElement])(
                (elems: Map[String, NetElement], elem: (String, NetElement)) => elems + elem)
    }

    def connectNodes[T <: NetElement](elements: Map[String, T]) = {
        val source = elements.find(elem => elem._2 match {
            case node: NetNode => node.source
            case _ => false
        }).get._2

        for ((key, value) <- elements if value.isInstanceOf[NetArc]) {
            val arc = value.asInstanceOf[NetArc]
            elements(arc.fromId).asInstanceOf[NetNode]
                .connectWith(elements(arc.toId).asInstanceOf[NetNode])
        }
        println(elements.count(elem => !elem._2.isInstanceOf[NetArc]) + " nodes")
    }
}
