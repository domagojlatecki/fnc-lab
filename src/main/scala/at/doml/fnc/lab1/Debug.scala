package at.doml.fnc.lab1

import java.text.DecimalFormat
import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.set.FuzzySet

object Debug {

    private val formatter = new DecimalFormat("0.000000")

    def print(domain: Domain, heading: String): Unit = {
        this.printHeadingAndDomainElements(heading, domain) {
            e => println(s"Domain element: $e")
        }

        println(s"Cardinality: ${domain.cardinality}")
        println()
    }

    def print(set: FuzzySet, heading: String): Unit = {
        this.printHeadingAndDomainElements(heading, set.domain) {
            e => println(s"d($e) = ${this.formatter.format(set.getValueAt(e))}")
        }

        println()
    }

    private def printHeadingAndDomainElements(heading: String, domain: Domain)(printFn: DomainElement => Unit): Unit = {
        if (heading != null) {
            println(heading)
        }

        for (e <- domain) {
            printFn(e)
        }
    }
}
