package at.doml.fnc.lab1

import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.relations.Relations
import at.doml.fnc.lab1.set.{FuzzySet, MutableFuzzySet}

object Task6 extends App {

    val u = Domain.intRange(1, 5)
    val r1 = new MutableFuzzySet(Domain.combine(u, u))
        .set(DomainElement.of(1, 1), 1)
        .set(DomainElement.of(2, 2), 1)
        .set(DomainElement.of(3, 3), 1)
        .set(DomainElement.of(4, 4), 1)
        .set(DomainElement.of(1, 2), 0.3)
        .set(DomainElement.of(2, 1), 0.3)
        .set(DomainElement.of(2, 3), 0.5)
        .set(DomainElement.of(3, 2), 0.5)
        .set(DomainElement.of(3, 4), 0.2)
        .set(DomainElement.of(4, 3), 0.2)
    var r2: FuzzySet = r1

    println(s"Initial relation is fuzzy equivalence relation? ${Relations.isFuzzyEquivalence(r2)}")
    println()

    for (i <- 1 to 3) {
        r2 = Relations.compositionOfBinaryRelations(r2, r1)

        println(s"Number of compositions: $i. Relation:")

        for (e <- r2.domain) {
            println(s"μ($e) = ${r2.getValueAt(e)}")
        }

        println(s"This relation is fuzzy equivalence relation? ${Relations.isFuzzyEquivalence(r2)}")
        println()
    }
}
