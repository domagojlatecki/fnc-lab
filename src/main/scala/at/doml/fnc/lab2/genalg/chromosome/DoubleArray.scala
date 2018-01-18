package at.doml.fnc.lab2.genalg.chromosome

class DoubleArray(val underlying: Array[Double]) {

    override def toString: String = {
        if (underlying.length <= 6) {
            underlying.mkString("[", ", ", "]")
        } else {
            underlying.take(6).mkString("[", ", ", ", ...]")
        }
    }
}
