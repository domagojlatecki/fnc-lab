package at.doml.fnc.lab1

import at.doml.fnc.lab1.domain.{Domain, DomainElement}

object Task1 extends App {

    val d1 = Domain.intRange(0, 5)
    Debug.print(d1, "Elements of domain d1:")

    val d2 = Domain.intRange(0, 3)
    Debug.print(d2, "Elements of domain d2:")

    val d3 = Domain.combine(d1, d2)
    Debug.print(d3, "Elements of domain d3:")

    println(d3.elementFor(index = 0))
    println(d3.elementFor(index = 5))
    println(d3.elementFor(index = 14))
    println(d3.indexOf(DomainElement.of(4, 1)))
}
