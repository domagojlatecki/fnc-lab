package at.doml.fnc.lab1

import at.doml.fnc.lab1.domain.{Domain, DomainElement}
import at.doml.fnc.lab1.set.{MutableFuzzySet, Operations}

object Task3 extends App {

    val d1 = Domain.intRange(0, 11)
    val set1 = new MutableFuzzySet(d1)
        .set(DomainElement.of(0), 1.0)
        .set(DomainElement.of(1), 0.8)
        .set(DomainElement.of(2), 0.6)
        .set(DomainElement.of(3), 0.4)
        .set(DomainElement.of(4), 0.2)
    Debug.print(set1, "Set1:")

    val notSet1 = Operations.unaryOperation(
        set1, Operations.zadehNot
    )
    Debug.print(notSet1, "not(Set1):")

    val union = Operations.binaryOperation(
        set1, notSet1,
        Operations.zadehOr
    )
    Debug.print(union, "Set1 ∪ notSet1:")

    val hinters = Operations.binaryOperation(
        set1, notSet1,
        Operations.hamacherTNorm(1.0)
    )
    Debug.print(hinters, "Set1 ∩ not(Set1) [using parametrised Hamacher T-norm with parameter 1.0]:")
}
