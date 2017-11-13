package at.doml.fnc.lab1.set

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

trait FuzzySet {

    val domain: Domain

    def getValueAt(element: DomainElement): Double

    override def toString: String = {
        val builder = new StringBuilder("{\n")

        for (element <- this.domain) {
            builder.append("    ")
                .append(element)
                .append(" / ")
                .append(this.getValueAt(element))
                .append('\n')
        }

        builder.append("}\n").toString()
    }
}
