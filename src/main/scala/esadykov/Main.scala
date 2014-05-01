package esadykov

/**
 * @author Ernest Sadykov
 * @since 01.05.2014
 */
object Main {
    def main(args: Array[String]) {
        val res = XmlNetReader.readNodes("net1.xml")
        println(res)
    }
}
