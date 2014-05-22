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
     * @param modelData uuid -> (x, y, width, height)
     * @return empty string if everything is fine, error description otherwise
     */
    def buildAdapters(modelData: Map[String, (Int, Int, Int, Int)],
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
            result = result + "\n" + """<arcs xsi:type="hlpn:ArcTP" uuid="%s" source="#%s" target="#%s"/>"""
                .format(inArcs(i)._1, inArcs(i)._2._1, inArcs(i)._2._2)
        }

        val middlePart = "\n</netSystem>\n</net>\n<diagramNetSystem uuid=\"%s\" model=\"#%s\">".format(uuid(), netSystemId)
        result = result + middlePart

        // print objects
        val modelTransitions: IndexedSeq[String] = for (i <- 1 to outputSockets.size) yield uuid()
        val modelOutputSockets: IndexedSeq[IndexedSeq[String]] =
            for (i <- 0 until outputSockets.size) yield
            // generate one
                for (j <- 0 until outputSockets(i).size) yield uuid()
        val modelInputSockets: IndexedSeq[String] =
            for (i <- 0 until inputSocket.size) yield uuid()
        val modelOutArcs: IndexedSeq[IndexedSeq[(String, (String, String))]] =
        // generate n
            for (i <- 0 until modelOutputSockets.size) yield
            // generate one
                for (j <- 0 until modelOutputSockets(i).size) yield uuid() -> (modelOutputSockets(i)(j), modelTransitions(i))

        val modelInArcs: IndexedSeq[(String, (String, String))] =
            for (i <- 0 until modelOutputSockets.size) yield
                (uuid(), (modelTransitions(i), modelInputSockets(i)))

        val TopMargin = 30
        val BetweenPlaces = 30
        var currentXCoord = 20
        for (i <- 0 until outputSockets.size) {
            for (j <- 0 until outputSockets(i).size) {
                val md: (Int, Int, Int, Int) = modelData(outputSockets(i)(j).uuid)
                result = result + "\n<nodes xsi:type=\"npndiagrams:NPNSymbolPlaceSN\" uuid=\"%s\" constraints=\"%d,%d,%d,%d\" model=\"#%s\" outArcs=\"#%s\"/>"
                    .format(modelOutputSockets(i)(j), currentXCoord, TopMargin, md._3, md._4, outputSockets(i)(j).uuid, modelOutArcs(i)(j)._1)
                currentXCoord = currentXCoord + md._3 + BetweenPlaces
            }
            result = result + "\n<nodes xsi:type=\"npndiagrams:NPNSymbolTransitionSN\" uuid=\"%s\" constraints=\"%d,%d,%d,%d\" model=\"#%s\" outArcs=\"#%s\" inArcs=\"#%s\"/>"
                .format(modelTransitions(i), currentXCoord - BetweenPlaces, TopMargin + 50, 30, 30, transitions(i), modelInArcs(i)._1, modelOutArcs(i).map(_._1).mkString(" #"))
            result = result + "\n<nodes xsi:type=\"npndiagrams:NPNSymbolPlaceSN\" uuid=\"%s\" constraints=\"%d,%d,%d,%d\" model=\"#%s\" inArcs=\"#%s\"/>"
                .format(modelInputSockets(i), currentXCoord - BetweenPlaces, TopMargin + 100, modelData(inputSocket(i).uuid)._3,
                    modelData(inputSocket(i).uuid)._4, inputSocket(i).uuid, modelInArcs(i)._1)

            for (j <- 0 until outputSockets(i).size) {
                result = result + "\n<arcs xsi:type=\"npndiagrams:NPNSymbolArcTPSN\" uuid=\"%s\" model=\"#%s\" target=\"#%s\" source=\"#%s\"/>"
                    .format(modelOutArcs(i)(j)._1, outArcs(i)(j)._1, modelOutArcs(i)(j)._2._2, modelOutArcs(i)(j)._2._1)
            }
            result = result + "\n<arcs xsi:type=\"npndiagrams:NPNSymbolArcTPSN\" uuid=\"%s\" model=\"#%s\" target=\"#%s\" source=\"#%s\"/>"
                .format(modelInArcs(i)._1, inArcs(i)._1, modelInArcs(i)._2._2, modelInArcs(i)._2._1)
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
