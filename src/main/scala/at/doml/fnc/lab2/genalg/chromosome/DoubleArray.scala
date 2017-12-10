package at.doml.fnc.lab2.genalg.chromosome

class DoubleArray(val underlying: Array[Double]) {

    override def toString: String = underlying.mkString("[", ", ", "]")
}
