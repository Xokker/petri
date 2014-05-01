package esadykov
import scala.xml.XML._
import scala.xml.NodeSeq
import scala.annotation.tailrec

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object XmlNetReader {
    def readNodes(filename: String): Map[String, NetNode] = {
        @tailrec
        def read(seq: NodeSeq, acc: Map[String, NetNode]): Map[String, NetNode] =
            if (seq.isEmpty) acc
            else {
                val uuid: String = (seq.head \ "@uuid")(0).text
                read(seq.tail, acc + (uuid -> new NetNode(uuid, (seq.head \ "@name")(0) text)))
            }

        read(((load(getClass.getResourceAsStream(filename)) \ "net") (0) \ "netSystem") (0) \ "nodes", Map.empty)
    }
}
