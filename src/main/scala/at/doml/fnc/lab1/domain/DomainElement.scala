package at.doml.fnc.lab1.domain

import java.util

final class DomainElement(elems: Array[Int]) {

    require(elems != null && elems.length > 0)

    private val values: Array[Int] = elems.clone()

    val numberOfComponents: Int = this.values.length

    def getComponentValue(index: Int) = this.values(index)

    def canEqual(other: Any): Boolean = other.isInstanceOf[DomainElement]

    override def equals(other: Any): Boolean = other match {
        case other: DomainElement => other.canEqual(this) && util.Arrays.equals(this.values, other.values)
        case _ => false
    }

    override def hashCode(): Int = util.Arrays.hashCode(this.values)

    override def toString: String = if (this.values.length == 1) {
        this.values(0).toString
    } else {
        this.values.mkString(start = "(", sep = ", ", end = ")")
    }
}

object DomainElement {

    def of(values: Int*): DomainElement = new DomainElement(values.toArray)
}
