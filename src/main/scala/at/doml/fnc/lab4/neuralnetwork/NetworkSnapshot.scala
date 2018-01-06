package at.doml.fnc.lab4.neuralnetwork

class NetworkSnapshot(xFuzzySets: List[FuzzySet], yFuzzySets: List[FuzzySet], rules: List[Rule], x: Double, y: Double) {

    val muX: List[Double] = xFuzzySets.map(_.apply(x))
    val muY: List[Double] = yFuzzySets.map(_.apply(y))
    val w: List[Double] = (for (i <- xFuzzySets.indices) yield {
        muX(i) * muY(i)
    }).toList
    val wSum: Double = w.sum
    val wBar: List[Double] = w.map(_ / wSum)
    val f: List[Double] = rules.map(_.apply(x, y))
    val wTimesFSum: Double = (for (i <- xFuzzySets.indices) yield {
        w(i) * f(i)
    }).sum
    val out: Double = (for (i <- xFuzzySets.indices) yield {
        wBar(i) * f(i)
    }).sum
}
