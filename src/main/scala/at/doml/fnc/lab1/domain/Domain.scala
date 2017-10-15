package at.doml.fnc.lab1.domain

import scala.collection.mutable.ListBuffer

trait Domain extends Iterable[DomainElement] {

    val cardinality: Int

    def getComponent(index: Int): Domain

    val numberOfComponents: Int

    def indexOf(element: DomainElement): Int

    def elementFor(index: Int): DomainElement

    override def iterator: Iterator[DomainElement] = Iterator.range(0, this.cardinality).map(this.elementFor)
}

object Domain {

    def intRange(from: Int, to: Int): Domain = new SimpleDomain(from, to)

    def combine(first: Domain, second: Domain): Domain = {
        val buffer = new ListBuffer[SimpleDomain]
        this.extractSimpleDomains(first, buffer)
        this.extractSimpleDomains(second, buffer)
        new CompositeDomain(buffer.toArray)
    }

    private def extractSimpleDomains(domain: Domain, buffer: ListBuffer[SimpleDomain]): Unit = {
        domain match {
            case s: SimpleDomain => {
                buffer += s
            }
            case c: CompositeDomain => {
                for (i <- 0 to c.numberOfComponents) {
                    buffer += c.getComponent(i).asInstanceOf[SimpleDomain]
                }
            }
            case _ => throw new UnsupportedOperationException
        }
    }
}
