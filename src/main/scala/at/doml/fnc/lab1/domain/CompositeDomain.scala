package at.doml.fnc.lab1.domain

final class CompositeDomain(elems: Array[SimpleDomain]) extends Domain {

    require(elems != null && elems.length > 0)

    private val subDomains: Array[SimpleDomain] = elems.clone()

    override val cardinality: Int = this.subDomains.map(_.cardinality).product

    override def getComponent(index: Int): Domain = this.subDomains(index)

    override val numberOfComponents: Int = this.subDomains.length

    override def indexOf(element: DomainElement): Int = {
        if (element.numberOfComponents != this.numberOfComponents) {
            return -1
        }

        var index = 0
        var previousCardinality = 1

        for {
            i <- (this.subDomains.length - 1) to 0 by -1
            subDomain = this.subDomains(i)
        } {
            val subIndex = subDomain.indexOf(DomainElement.of(element.getComponentValue(i)))

            if (subIndex == -1) {
                return -1
            }

            index += subIndex * previousCardinality
            previousCardinality *= subDomain.cardinality
        }

        index
    }

    override def elementFor(index: Int): DomainElement = {
        if (index < 0 || index >= this.cardinality) {
            throw new ArrayIndexOutOfBoundsException(index)
        }

        var div = 1
        val elems: Array[Int] = (for {
            i <- (this.subDomains.length - 1) to 0 by -1
        } yield {
            val subDomain = this.subDomains(i)
            val lastElement = subDomain.elementFor(index / div % subDomain.cardinality).getComponentValue(0)

            div *= subDomain.cardinality
            lastElement
        }).reverse.toArray

        DomainElement.of(elems: _*)
    }
}
