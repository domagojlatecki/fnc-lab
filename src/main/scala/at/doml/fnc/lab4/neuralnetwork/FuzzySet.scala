package at.doml.fnc.lab4.neuralnetwork

class FuzzySet {

    var a: Double = Math.random()
    var b: Double = Math.random()

    def apply(x: Double): Double = 1.0 / (1.0 + Math.exp(b * (x - a)))
}
