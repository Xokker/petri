package esadykov
import scala.xml.XML._
import esadykov.nets.{NetElement, NetNode, NetArc}
import java.io.{FileNotFoundException, FileInputStream, InputStream}

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object XmlNetManager {
    /**
     * Reads net from text file (NPNTool extension).
     *
     * @param filename full path to file or short name for loading from classpath
     * @return pair: elements of the map (key: uuid, value: element)
     *         and name of the net
     */
    def readNetElements(filename: String): (Map[String, NetElement], String) = {
        val stream: InputStream = try new FileInputStream(filename)
                                  catch {
                                      case _: FileNotFoundException =>
                                          getClass.getResourceAsStream(filename)
                                  }
        val netSystem = ((load(stream)
            \ "net") (0) \ "netSystem") (0)
        val elements = (netSystem \ "nodes").map(node => new NetNode((node \ "@uuid").text, (node \ "@name").text)) ++
            (netSystem \ "arcs").map(arc => new NetArc((arc \ "@uuid").text, (arc \ "@source").text.drop(1), (arc \ "@target").text.drop(1)))
        (elements.map(elem => elem.uuid -> elem)
            .foldLeft(Map.empty[String, NetElement])(
                (elems: Map[String, NetElement], elem: (String, NetElement)) => elems + elem), (netSystem \ "@name").text)
    }

    def connectNodes[T <: NetElement](elements: Map[String, T]): Unit =
        for ((key, value) <- elements if value.isInstanceOf[NetArc]) {
            val arc = value.asInstanceOf[NetArc]
            elements(arc.fromId)
                .asInstanceOf[NetNode]
                .connectWith(elements(arc.toId).asInstanceOf[NetNode])
        }
}
