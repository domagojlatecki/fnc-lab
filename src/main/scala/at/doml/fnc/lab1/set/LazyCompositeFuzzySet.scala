package at.doml.fnc.lab1.set

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

class LazyCompositeFuzzySet(d: Domain, function: DomainElement => Double) extends FuzzySet {

    override val domain: Domain = d

    override def getValueAt(element: DomainElement): Double = function(element)
}
