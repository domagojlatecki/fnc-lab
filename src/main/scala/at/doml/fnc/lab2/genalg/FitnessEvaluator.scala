package at.doml.fnc.lab2.genalg

trait FitnessEvaluator[T] {

    def evaluate(chromosome: T): Double
}
