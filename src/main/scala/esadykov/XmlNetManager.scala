package esadykov

import scala.xml.XML._
import esadykov.nets.{NetElement, NetNode, NetArc}
import java.io.{FileNotFoundException, FileInputStream, InputStream}
import scala.xml.{Elem, Node}
import scala.collection.immutable.Seq

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
    def readNetElements(filename: String): (Map[String, NetElement], String, Map[String, (Int, Int, Int, Int)]) = {
        val stream: InputStream = try new FileInputStream(filename)
                                  catch {
                                      case _: FileNotFoundException =>
                                          getClass.getResourceAsStream(filename)
                                  }
        val loaded: Elem = load(stream)
        val netSystem: Node = ((loaded \ "net") (0) \ "netSystem") (0)
        val elements = (netSystem \ "nodes").map(node => new NetNode((node \ "@uuid").text, (node \ "@name").text)) ++
            (netSystem \ "arcs").map(arc => new NetArc((arc \ "@uuid").text, (arc \ "@source").text.drop(1), (arc \ "@target").text.drop(1)))


        val diagramNetSystem = (loaded \ "diagramNetSystem") (0)
        val models: Seq[(String, (Int, Int, Int, Int))] = (diagramNetSystem \ "nodes").map(node => ((node \ "@model").text.stripPrefix("#"), extractCoord((node \ "@constraints").text)))
        val modelMap: Map[String, (Int, Int, Int, Int)] = models.foldLeft(Map.empty[String, (Int, Int, Int, Int)])((m, pair) => m + (pair._1 -> pair._2))

        (elements.map(elem => elem.uuid -> elem)
            .foldLeft(Map.empty[String, NetElement])(
                (elems: Map[String, NetElement], elem: (String, NetElement)) => elems + elem), (netSystem \ "@name").text, modelMap)
    }

    private def extractCoord(s: String): (Int, Int, Int, Int) = {
        val ar: Array[String] = s.split(',')
        (ar(0).trim.toInt, ar(1).trim.toInt, ar(2).trim.toInt, ar(3).trim.toInt)
    }

    def connectNodes[T <: NetElement](elements: Map[String, T]): Unit =
        for ((key, value) <- elements if value.isInstanceOf[NetArc]) {
            val arc = value.asInstanceOf[NetArc]
            elements(arc.fromId)
                .asInstanceOf[NetNode]
                .connectWith(elements(arc.toId).asInstanceOf[NetNode])
        }
}
