package esadykov.nets

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
class NetElement(val uuid: String) {
    override def toString =
        getClass.getSimpleName+"[uuid="+uuid+"]"

    override def equals(that: Any) =
        that.isInstanceOf[NetElement] && that.asInstanceOf[NetElement].uuid == this.uuid

    override def hashCode = uuid.hashCode
}
