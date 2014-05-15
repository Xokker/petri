package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
case class Output(id: String) extends Expression {
    override def toString = "!"+id

}
