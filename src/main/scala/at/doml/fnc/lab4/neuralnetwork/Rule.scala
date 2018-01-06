package at.doml.fnc.lab4.neuralnetwork

class Rule {

    var p: Double = Math.random()
    var q: Double = Math.random()
    var r: Double = Math.random()

    def apply(x: Double, y: Double): Double = p * x + q * y + r
}
