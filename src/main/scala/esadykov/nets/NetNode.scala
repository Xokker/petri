package esadykov.nets


/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetNode(uuid: String, _name: String) extends NetElement(uuid) {
    private[this] val InputPrefix = "I:"
    private[this] val OutputPrefix = "O:"

    val input: Boolean = _name.startsWith(InputPrefix)
    val output: Boolean = _name.startsWith(OutputPrefix)
    val source: Boolean = _name == "source"
    val sink: Boolean = _name == "sink"
    var connections: Array[NetNode] = Array.empty[NetNode]

    def connectWith(anotherNode: NetNode): Unit =
        connections = connections :+ anotherNode

    def name: String =
        _name.stripPrefix(InputPrefix).stripPrefix(OutputPrefix)

    override def toString =
        getClass.getSimpleName+"[uuid="+uuid+",name="+name+"," +
            "input="+input+",output="+output+"]"
}
