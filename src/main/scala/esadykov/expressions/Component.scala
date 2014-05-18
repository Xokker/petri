package esadykov.expressions

/**
 * @author Ernest Sadykov
 * @since 04.05.2014
 */
class Component(val id: String, val stars: Int = 0, val withoutStar: Int = 0) {

    def incrementStars: Component = new Component(id, stars + 1, withoutStar)

    def incrementWithoutStars: Component = new Component(id, stars, withoutStar + 1)

    override def equals(that: Any) =
        if (that == null || !that.isInstanceOf[Component]) false
        else that.asInstanceOf[Component].id.equals(id)


    override def hashCode(): Int = {
        val state = Seq(id)
        state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
    }

    override def toString =
        "Component[id="+id+",stars="+stars+",withoutStar="+withoutStar+"]"

}
