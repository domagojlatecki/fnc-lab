package at.doml.fnc.lab1.set

object Operations {

    def unaryOperation(set: FuzzySet, operation: Double => Double): FuzzySet = {
        val result = new MutableFuzzySet(set.domain)

        for (i <- 0 until set.domain.cardinality) {
            val element = set.domain.elementFor(i)

            result.set(element, operation(set.getValueAt(element)))
        }

        result
    }

    def binaryOperation(first: FuzzySet, second: FuzzySet, operation: (Double, Double) => Double): FuzzySet = {
        if (first.domain.cardinality != second.domain.cardinality
            || first.domain.numberOfComponents != second.domain.numberOfComponents) {
            throw new IllegalArgumentException("Unable to perform binary operation on given sets")
        }

        val result = new MutableFuzzySet(first.domain)

        for (i <- 0 until first.domain.cardinality) {
            val element = first.domain.elementFor(i)

            result.set(element, operation(first.getValueAt(element), second.getValueAt(element)))
        }

        result
    }

    val zadehNot: Double => Double = a => 1.0 - a
    val zadehAnd: (Double, Double) => Double = (a, b) => a min b
    val zadehOr: (Double, Double) => Double = (a, b) => a max b

    def hamacherTNorm(nu: Double): (Double, Double) => Double =
        (a, b) => a * b / (nu + (1 - nu) * (a + b - a * b))

    def hamacherSNorm(nu: Double): (Double, Double) => Double =
        (a, b) => (a + b - (2 - nu) * a * b) / (1 - (1 - nu) * a * b)
}
