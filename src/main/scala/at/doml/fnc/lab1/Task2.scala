package at.doml.fnc.lab1

import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.set.{CalculatedFuzzySet, MutableFuzzySet, StandardFuzzySets}

object Task2 extends App {

    val d1 = Domain.intRange(0, 11)
    val set1 = new MutableFuzzySet(d1)
        .set(DomainElement.of(0), 1.0)
        .set(DomainElement.of(1), 0.8)
        .set(DomainElement.of(2), 0.6)
        .set(DomainElement.of(3), 0.4)
        .set(DomainElement.of(4), 0.2)
    Debug.print(set1, "Set1:")

    val d2 = Domain.intRange(-5, 6)
    val set2 = new CalculatedFuzzySet(
        d2,
        StandardFuzzySets.lambdaFunction(
            d2.indexOf(DomainElement.of(-4)),
            d2.indexOf(DomainElement.of(0)),
            d2.indexOf(DomainElement.of(4))
        )
    )
    Debug.print(set2, "Set2:")
}
