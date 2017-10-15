package at.doml.fnc.lab1.set

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

trait FuzzySet {

    val domain: Domain

    def getValueAt(element: DomainElement): Double
}
