package at.doml.fnc.lab1.set

object Operations {

    def unaryOperation(set: FuzzySet, operation: Double => Double): FuzzySet =
        new CalculatedFuzzySet(set.domain, i => operation(set.getValueAt(set.domain.elementFor(i))))

    def binaryOperation(first: FuzzySet, second: FuzzySet, operation: (Double, Double) => Double): FuzzySet = {
        if (first.domain.cardinality != second.domain.cardinality
            || first.domain.numberOfComponents != second.domain.numberOfComponents) {
            throw new IllegalArgumentException("Unable to perform binary operation on given sets")
        }

        new CalculatedFuzzySet(first.domain, i => {
            val element = first.domain.elementFor(i)
            operation(first.getValueAt(element), second.getValueAt(element))
        })
    }

    val zadehNot: Double => Double = a => 1.0 - a
    val zadehAnd: (Double, Double) => Double = (a, b) => a min b
    val zadehOr: (Double, Double) => Double = (a, b) => a max b

    def hamacherTNorm(nu: Double): (Double, Double) => Double =
        (a, b) => a * b / (nu + (1 - nu) * (a + b - a * b))

    def hamacherSNorm(nu: Double): (Double, Double) => Double =
        (a, b) => (a + b - (2 - nu) * a * b) / (1 - (1 - nu) * a * b)
}
