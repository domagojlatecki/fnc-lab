package at.doml.fnc.lab1

import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.relations.Relations
import at.doml.fnc.lab1.set.MutableFuzzySet

object Task4 extends App {

    val u = Domain.intRange(1, 6)
    val u2 = Domain.combine(u, u)
    val r1 = new MutableFuzzySet(u2)
        .set(DomainElement.of(1, 1), 1)
        .set(DomainElement.of(2, 2), 1)
        .set(DomainElement.of(3, 3), 1)
        .set(DomainElement.of(4, 4), 1)
        .set(DomainElement.of(5, 5), 1)
        .set(DomainElement.of(3, 1), 0.5)
        .set(DomainElement.of(1, 3), 0.5)
    val r2 = new MutableFuzzySet(u2)
        .set(DomainElement.of(1, 1), 1)
        .set(DomainElement.of(2, 2), 1)
        .set(DomainElement.of(3, 3), 1)
        .set(DomainElement.of(4, 4), 1)
        .set(DomainElement.of(5, 5), 1)
        .set(DomainElement.of(3, 1), 0.5)
        .set(DomainElement.of(1, 3), 0.1)
    val r3 = new MutableFuzzySet(u2)
        .set(DomainElement.of(1, 1), 1)
        .set(DomainElement.of(2, 2), 1)
        .set(DomainElement.of(3, 3), 0.3)
        .set(DomainElement.of(4, 4), 1)
        .set(DomainElement.of(5, 5), 1)
        .set(DomainElement.of(1, 2), 0.6)
        .set(DomainElement.of(2, 1), 0.6)
        .set(DomainElement.of(2, 3), 0.7)
        .set(DomainElement.of(3, 2), 0.7)
        .set(DomainElement.of(3, 1), 0.5)
        .set(DomainElement.of(1, 3), 0.5)
    val r4 = new MutableFuzzySet(u2)
        .set(DomainElement.of(1, 1), 1)
        .set(DomainElement.of(2, 2), 1)
        .set(DomainElement.of(3, 3), 1)
        .set(DomainElement.of(4, 4), 1)
        .set(DomainElement.of(5, 5), 1)
        .set(DomainElement.of(1, 2), 0.4)
        .set(DomainElement.of(2, 1), 0.4)
        .set(DomainElement.of(2, 3), 0.5)
        .set(DomainElement.of(3, 2), 0.5)
        .set(DomainElement.of(1, 3), 0.4)
        .set(DomainElement.of(3, 1), 0.4)

    val test1 = Relations.isUTimesURelation(r1)
    println(s"r1 is defined over UxU? $test1")

    val test2 = Relations.isSymmetric(r1)
    println(s"r1 is symmetric? $test2")

    val test3 = Relations.isSymmetric(r2)
    println(s"r2 is symmetric? $test3")

    val test4 = Relations.isReflexive(r1)
    println(s"r1 is reflexive? $test4")

    val test5 = Relations.isReflexive(r3)
    println(s"r3 is reflexive? $test5")

    val test6 = Relations.isMaxMinTransitive(r3)
    println(s"r3 is max-min transitive? $test6")

    val test7 = Relations.isMaxMinTransitive(r4)
    println(s"r4 is max-min transitive? $test7")
}
