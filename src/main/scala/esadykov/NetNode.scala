package esadykov

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetNode(val uuid: String, val name: String) {
    override def toString =
        "NetNode[uuid="+uuid+",name="+name+"]"
}
