package at.doml.fnc.lab1.domain

final class SimpleDomain(from: Int, to: Int) extends Domain {

    require(from < to)

    val start: Int = from
    val end: Int = to

    override val cardinality: Int = to - from

    override def getComponent(index: Int): Domain = {
        if (index != 0) {
            throw new ArrayIndexOutOfBoundsException(index)
        }

        this
    }

    override val numberOfComponents: Int = 1

    override def indexOf(element: DomainElement): Int = {
        if (element.numberOfComponents != this.numberOfComponents) {
            return -1
        }

        val component = element.getComponentValue(0)

        if (component < this.start || component >= this.end) {
            return -1
        }

        component - this.start
    }

    override def elementFor(index: Int): DomainElement = {
        if (index < 0 || index >= this.cardinality) {
            throw new ArrayIndexOutOfBoundsException(index)
        }

        DomainElement.of(from + index)
    }
}
