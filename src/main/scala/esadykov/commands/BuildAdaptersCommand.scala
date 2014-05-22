package esadykov.commands

import esadykov.nets.NetNode
import java.util.UUID
import java.io.FileWriter

/**
 * @author Ernest Sadykov
 * @since 22.05.2014
 */
class BuildAdaptersCommand(val path: String) extends Command {
    private def uuid() = "npn" + UUID.randomUUID().toString

    /**
     * @return empty string if everything is fine, error description otherwise
     */
    def buildAdapters(modelData: Map[NetNode, (Int, Int, Int, Int)],
                      outputSockets: IndexedSeq[IndexedSeq[NetNode]], inputSocket: IndexedSeq[NetNode]): String = {
        val netSystemId = uuid()
        val header = """<?xml version="1.0" encoding="UTF-8"?>
            <npnets:NPnetMarked xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:hlpn="mathtech.ru/npntool/hlpn" xmlns:npndiagrams="http:/mathtech.ru/npntool/npndiagrams" xmlns:npnets="mathtech.ru/npntool/npnets" uuid="npn02a2c105-d740-4855-9372-d37c6cd7573a">
                <net uuid="%s" name="Adapters">
                    <netSystem uuid="%s" name="Adapters">""".format(uuid(), netSystemId)
        var result: String = header

        val transitions: IndexedSeq[String] = for (i <- 1 to outputSockets.size) yield uuid()
        // uuid -> (formUUID, toUUID)
        val outArcs: IndexedSeq[IndexedSeq[(String, (String, String))]] =
        // generate n
            for (i <- 0 until outputSockets.size) yield
            // generate one
                for (node <- outputSockets(i)) yield uuid() -> (node.uuid, transitions(i))

        val inArcs: IndexedSeq[(String, (String, String))] =
            for (i <- 0 until outputSockets.size) yield
                (uuid(), (transitions(i), inputSocket(i).uuid))

        // declare objects
        for (i <- 0 until outputSockets.size) {
            for (j <- 0 until outputSockets(i).size) {
                val node: NetNode = outputSockets(i)(j)
                result = result + "\n" + """<nodes xsi:type="hlpn:Place" uuid="%s" name="%s" outArcs="#%s"/>""".format(node.uuid, node.name, outArcs(i)(j)._1)
            }
            result = result + "\n" + """<nodes xsi:type="hlpn:Transition" uuid="%s" name="%s" inArcs="#%s" outArcs="#%s"/>""".format(transitions(i), "t"+i, outArcs(i).map(_._1).mkString(" #"), inArcs(i)._1)
            result = result + "\n" + """<nodes xsi:type="hlpn:Place" uuid="%s" name="%s" inArcs="#%s"/>""".format(inputSocket(i).uuid, inputSocket(i).realName, inArcs(i)._1)
            for (j <- 0 until outputSockets(i).size) {
                val currentArc: (String, (String, String)) = outArcs(i)(j)
                result = result + "\n" + """<arcs xsi:type="hlpn:ArcTP" uuid="%s" source="#%s" target="#%s"/>""".format(currentArc._1, currentArc._2._1, currentArc._2._1)
            }
        }

        val middlePart = "\n</netSystem>\n</net>\n<diagramNetSystem uuid=\"%s\" model=\"#%s\">".format(uuid(), netSystemId)
        result = result + middlePart

        // print objects
        for (i <- 0 until outputSockets.size) {

        }

        val bottom = "\n</diagramNetSystem>\n</npnets:NPnetMarked>"
        result = result + bottom

        val writer: FileWriter = new FileWriter(path)
        try {
            writer.write(result)
        } finally {
            writer.close()
        }

        ""
    }
}
