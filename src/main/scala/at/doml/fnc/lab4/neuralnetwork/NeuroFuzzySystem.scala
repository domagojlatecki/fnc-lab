package at.doml.fnc.lab4.neuralnetwork

class NeuroFuzzySystem(val numRules: Int) {

    private val xFuzzySets = (for (_ <- 0 until numRules) yield {
        new FuzzySet
    }).toList
    private val yFuzzySets = (for (_ <- 0 until numRules) yield {
        new FuzzySet
    }).toList
    private val rules = (for (_ <- 0 until numRules) yield {
        new Rule
    }).toList

    def apply(x: Double, y: Double): Double = this.snapshot(x, y).out

    def snapshot(x: Double, y: Double): NetworkSnapshot = new NetworkSnapshot(xFuzzySets, yFuzzySets, rules, x, y)

    def p(i: Int): Double = rules(i).p

    def p_=(i: Int, p: Double): Unit = rules(i).p = p

    def q(i: Int): Double = rules(i).q

    def q_=(i: Int, q: Double): Unit = rules(i).q = q

    def r(i: Int): Double = rules(i).r

    def r_=(i: Int, r: Double): Unit = rules(i).r = r

    def ax(i: Int): Double = xFuzzySets(i).a

    def ax_=(i: Int, a: Double): Unit = xFuzzySets(i).a = a

    def bx(i: Int): Double = xFuzzySets(i).b

    def bx_=(i: Int, b: Double): Unit = xFuzzySets(i).b = b

    def ay(i: Int): Double = yFuzzySets(i).a

    def ay_=(i: Int, a: Double): Unit = yFuzzySets(i).a = a

    def by(i: Int): Double = yFuzzySets(i).b

    def by_=(i: Int, b: Double): Unit = yFuzzySets(i).b = b
}
