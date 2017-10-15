package at.doml.fnc.lab1.set

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

class CalculatedFuzzySet(d: Domain, function: Int => Double) extends FuzzySet {

    override val domain: Domain = d

    override def getValueAt(element: DomainElement): Double = {
        val index = d.indexOf(element)

        if (index == -1) {
            throw new NoSuchElementException(element.toString)
        }

        function(index)
    }
}
