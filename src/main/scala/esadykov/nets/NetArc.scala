package esadykov.nets

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetArc(uuid: String, val fromId: String, val toId: String) extends NetElement(uuid) {
    override def toString =
        getClass.getSimpleName+"[uuid="+uuid+",fromId="+fromId+",toId="+toId+"]"
}
