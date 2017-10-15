package at.doml.fnc.lab1.set

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

class MutableFuzzySet(d: Domain) extends FuzzySet {

    override val domain: Domain = d
    private val memberships: Array[Double] = new Array(d.cardinality)

    override def getValueAt(element: DomainElement): Double = this.memberships(d.indexOf(element))

    def set(element: DomainElement, value: Double): MutableFuzzySet = {
        if (value < 0 || value > 1) {
            throw new IllegalArgumentException(s"illegal value: $value")
        }

        val index = d.indexOf(element)

        if (index == -1) {
            throw new NoSuchElementException(element.toString)
        }

        this.memberships(index) = value
        this
    }
}
